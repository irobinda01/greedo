;; Dispute Resolution System
;; A DAO-based dispute resolution system for a lending protocol

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-INVALID-DISPUTE u2)
(define-constant ERR-INVALID-STATE u3)
(define-constant ERR-ALREADY-VOTED u4)
(define-constant ERR-VOTING-ENDED u5)
(define-constant ERR-VOTING-NOT-ENDED u6)
(define-constant ERR-QUORUM-NOT-REACHED u7)
(define-constant ERR-NO-RESOLUTION u8)
(define-constant ERR-FUNDS-RELEASED u9)
(define-constant ERR-NO-VOTING-POWER u10)
(define-constant ERR-INSUFFICIENT-FEE u11)
(define-constant ERR-INVALID-PERIOD u12)
(define-constant ERR-INVALID-PERCENTAGE u13)
(define-constant ERR-TRANSFER-FAILED u14)
(define-constant ERR-NO-FUNDS u15)
(define-constant ERR-TOKEN-CONTRACT-NOT-SET u1000)

;; Data variables
(define-data-var dispute-count uint u0)
(define-data-var dispute-fee uint u1000000) ;; in microSTX
(define-data-var quorum-percentage uint u10) ;; 10%
(define-data-var majority-percentage uint u51) ;; 51%
(define-data-var admin principal tx-sender)
(define-data-var governance-token-contract (optional principal) none)

;; Constants
(define-constant MINIMUM-VOTING-PERIOD u259200) ;; 3 days in seconds

;; Dispute status enum
(define-constant STATUS-CREATED u0)
(define-constant STATUS-VOTING u1)
(define-constant STATUS-RESOLVED u2)
(define-constant STATUS-CANCELED u3)

;; Resolution enum
(define-constant RESOLUTION-NONE u0)
(define-constant RESOLUTION-LENDER-WINS u1)
(define-constant RESOLUTION-BORROWER-WINS u2)

;; Maps
(define-map disputes 
  uint 
  {
    creator: principal,
    lender: principal,
    borrower: principal,
    loan-id: uint,
    description: (string-utf8 500),
    amount: uint,
    created-at: uint,
    voting-ends-at: uint,
    status: uint,
    resolution: uint,
    votes-for-lender: uint,
    votes-for-borrower: uint,
    funds-released: bool,
    evidence: (string-utf8 1000)
  }
)

(define-map dispute-escrow uint uint)

(define-map votes 
  { dispute-id: uint, voter: principal } 
  { has-voted: bool, supports-lender: bool }
)

(define-map dao-members principal bool)

;; Governance token interface
(define-trait governance-token-trait
  (
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
  )
)

;; Read-only functions for token interaction
;; Get token balance of a principal - using default-to instead of unwrap!
(define-read-only (get-token-balance (token-contract principal) (owner principal))
  (let
    (
      (balance-response (contract-call? token-contract get-balance owner))
    )
    (default-to u0 (get-ok balance-response))
  )
)

;; Get total token supply - using default-to instead of unwrap!
(define-read-only (get-total-supply (token-contract principal))
  (let
    (
      (supply-response (contract-call? token-contract get-total-supply))
    )
    (default-to u0 (get-ok supply-response))
  )
)

;; Check if an address is a DAO member
(define-read-only (is-dao-member (address principal))
  (default-to false (map-get? dao-members address))
)

;; Get dispute details
(define-read-only (get-dispute-details (dispute-id uint))
  (map-get? disputes dispute-id)
)

;; Get vote information
(define-read-only (get-vote-info (dispute-id uint) (voter principal))
  (map-get? votes { dispute-id: dispute-id, voter: voter })
)

;; Functions

;; Initialize the contract
(define-public (initialize (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (ok (var-set governance-token-contract (some token-contract)))
  )
)

;; Create a new dispute
(define-public (create-dispute 
    (lender principal)
    (borrower principal)
    (loan-id uint)
    (description (string-utf8 500))
    (voting-period uint)
    (evidence (string-utf8 1000))
  )
  (let
    (
      (dispute-id (var-get dispute-count))
      (current-time (unwrap! (get-block-info? time (- block-height u1)) u0))
    )
    ;; Check preconditions
    (asserts! (>= (stx-get-balance tx-sender) (var-get dispute-fee)) (err ERR-INSUFFICIENT-FEE))
    (asserts! (or (is-eq tx-sender lender) (is-eq tx-sender borrower)) (err ERR-NOT-AUTHORIZED))
    (asserts! (>= voting-period MINIMUM-VOTING-PERIOD) (err ERR-INVALID-PERIOD))
    
    ;; Create the dispute
    (map-set disputes dispute-id {
      creator: tx-sender,
      lender: lender,
      borrower: borrower, 
      loan-id: loan-id,
      description: description,
      amount: u0,
      created-at: current-time,
      voting-ends-at: (+ current-time voting-period),
      status: STATUS-CREATED,
      resolution: RESOLUTION-NONE,
      votes-for-lender: u0,
      votes-for-borrower: u0,
      funds-released: false,
      evidence: evidence
    })
    
    ;; Pay dispute fee
    (unwrap! (stx-transfer? (var-get dispute-fee) tx-sender (as-contract tx-sender)) (err ERR-TRANSFER-FAILED))
    
    ;; Increment dispute counter
    (var-set dispute-count (+ dispute-id u1))
    
    ;; Emit event
    (print {
      event: "dispute-created",
      dispute-id: dispute-id,
      creator: tx-sender,
      lender: lender,
      borrower: borrower,
      loan-id: loan-id
    })
    
    (ok dispute-id)
  )
)

;; Escrow funds for a dispute and start voting
(define-public (escrow-dispute-funds (dispute-id uint) (amount uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
    )
    ;; Check preconditions
    (asserts! (is-eq (get status dispute) STATUS-CREATED) (err ERR-INVALID-STATE))
    (asserts! (or (is-eq tx-sender (get lender dispute)) (is-eq tx-sender (get borrower dispute))) (err ERR-NOT-AUTHORIZED))
    (asserts! (> amount u0) (err ERR-NO-FUNDS))
    
    ;; Transfer funds to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) (err ERR-TRANSFER-FAILED))
    
    ;; Update dispute status
    (map-set disputes dispute-id (merge dispute {
      amount: amount,
      status: STATUS-VOTING
    }))
    
    ;; Record funds in escrow
    (map-set dispute-escrow dispute-id amount)
    
    ;; Emit event
    (print {
      event: "funds-escrowed",
      dispute-id: dispute-id,
      amount: amount
    })
    
    (ok true)
  )
)

;; Submit additional evidence for a dispute
(define-public (submit-evidence (dispute-id uint) (new-evidence (string-utf8 500)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
      (combined-evidence (concat (get evidence dispute) "\n---\n" new-evidence))
    )
    ;; Check preconditions
    (asserts! (or (is-eq (get status dispute) STATUS-CREATED) (is-eq (get status dispute) STATUS-VOTING)) (err ERR-INVALID-STATE))
    (asserts! (or (is-eq tx-sender (get lender dispute)) (is-eq tx-sender (get borrower dispute))) (err ERR-NOT-AUTHORIZED))
    
    ;; Update evidence
    (map-set disputes dispute-id (merge dispute {
      evidence: combined-evidence
    }))
    
    ;; Emit event
    (print {
      event: "evidence-submitted",
      dispute-id: dispute-id,
      submitter: tx-sender,
      evidence: new-evidence
    })
    
    (ok true)
  )
)

;; Cast a vote in a dispute
(define-public (cast-vote (dispute-id uint) (supports-lender bool))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
      (current-time (unwrap! (get-block-info? time (- block-height u1)) u0))
      (token-contract (unwrap! (var-get governance-token-contract) (err ERR-TOKEN-CONTRACT-NOT-SET)))
      (vote-key { dispute-id: dispute-id, voter: tx-sender })
      (token-balance (get-token-balance token-contract tx-sender))
    )
    ;; Check preconditions
    (asserts! (is-eq (get status dispute) STATUS-VOTING) (err ERR-INVALID-STATE))
    (asserts! (< current-time (get voting-ends-at dispute)) (err ERR-VOTING-ENDED))
    (asserts! (is-dao-member tx-sender) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-none (map-get? votes vote-key)) (err ERR-ALREADY-VOTED))
    (asserts! (> token-balance u0) (err ERR-NO-VOTING-POWER))
    
    ;; Record the vote
    (map-set votes vote-key { has-voted: true, supports-lender: supports-lender })
    
    ;; Update vote tallies
    (if supports-lender
      (map-set disputes dispute-id (merge dispute {
        votes-for-lender: (+ (get votes-for-lender dispute) token-balance)
      }))
      (map-set disputes dispute-id (merge dispute {
        votes-for-borrower: (+ (get votes-for-borrower dispute) token-balance)
      }))
    )
    
    ;; Emit event
    (print {
      event: "vote-cast",
      dispute-id: dispute-id,
      voter: tx-sender,
      supports-lender: supports-lender,
      vote-weight: token-balance
    })
    
    (ok true)
  )
)

;; Finalize a dispute after voting period ends
(define-public (finalize-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
      (current-time (unwrap! (get-block-info? time (- block-height u1)) u0))
      (token-contract (unwrap! (var-get governance-token-contract) (err ERR-TOKEN-CONTRACT-NOT-SET)))
      (total-votes (+ (get votes-for-lender dispute) (get votes-for-borrower dispute)))
      (total-supply (get-total-supply token-contract))
      (required-quorum (/ (* total-supply (var-get quorum-percentage)) u100))
      (resolution RESOLUTION-NONE)
    )
    ;; Check preconditions
    (asserts! (is-eq (get status dispute) STATUS-VOTING) (err ERR-INVALID-STATE))
    (asserts! (>= current-time (get voting-ends-at dispute)) (err ERR-VOTING-NOT-ENDED))
    (asserts! (>= total-votes required-quorum) (err ERR-QUORUM-NOT-REACHED))
    
    ;; Determine the winner
    (if (> (get votes-for-lender dispute) (get votes-for-borrower dispute))
      (let
        (
          (lender-percentage (/ (* (get votes-for-lender dispute) u100) total-votes))
        )
        (if (>= lender-percentage (var-get majority-percentage))
          (set resolution RESOLUTION-LENDER-WINS)
          (set resolution RESOLUTION-NONE)
        )
      )
      (if (> (get votes-for-borrower dispute) (get votes-for-lender dispute))
        (let
          (
            (borrower-percentage (/ (* (get votes-for-borrower dispute) u100) total-votes))
          )
          (if (>= borrower-percentage (var-get majority-percentage))
            (set resolution RESOLUTION-BORROWER-WINS)
            (set resolution RESOLUTION-NONE)
          )
        )
        (set resolution RESOLUTION-NONE)
      )
    )
    
    ;; Update dispute status
    (map-set disputes dispute-id (merge dispute {
      status: STATUS-RESOLVED,
      resolution: resolution
    }))
    
    ;; Emit event
    (print {
      event: "dispute-resolved",
      dispute-id: dispute-id,
      resolution: resolution
    })
    
    (ok resolution)
  )
)

;; Release escrowed funds based on dispute resolution
(define-public (release-funds (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
      (amount (default-to u0 (map-get? dispute-escrow dispute-id)))
      (recipient (if (is-eq (get resolution dispute) RESOLUTION-LENDER-WINS)
                   (get lender dispute)
                   (if (is-eq (get resolution dispute) RESOLUTION-BORROWER-WINS)
                     (get borrower dispute)
                     none)))
    )
    ;; Check preconditions
    (asserts! (is-eq (get status dispute) STATUS-RESOLVED) (err ERR-INVALID-STATE))
    (asserts! (not (get funds-released dispute)) (err ERR-FUNDS-RELEASED))
    (asserts! (not (is-eq (get resolution dispute) RESOLUTION-NONE)) (err ERR-NO-RESOLUTION))
    (asserts! (> amount u0) (err ERR-NO-FUNDS))
    (asserts! (is-some recipient) (err ERR-NO-RESOLUTION))
    
    ;; Mark funds as released
    (map-set disputes dispute-id (merge dispute { funds-released: true }))
    (map-set dispute-escrow dispute-id u0)
    
    ;; Transfer funds to recipient
    (unwrap! (as-contract (stx-transfer? amount tx-sender (unwrap! recipient (err ERR-NO-RESOLUTION)))) (err ERR-TRANSFER-FAILED))
    
    ;; Emit event
    (print {
      event: "funds-released",
      dispute-id: dispute-id,
      recipient: (unwrap! recipient (err ERR-NO-RESOLUTION)),
      amount: amount
    })
    
    (ok true)
  )
)

;; Cancel a dispute (only possible before voting starts)
(define-public (cancel-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) (err ERR-INVALID-DISPUTE)))
    )
    ;; Check preconditions
    (asserts! (is-eq (get status dispute) STATUS-CREATED) (err ERR-INVALID-STATE))
    (asserts! (or (is-eq tx-sender (get creator dispute)) (is-eq tx-sender (var-get admin))) (err ERR-NOT-AUTHORIZED))
    
    ;; Update dispute status
    (map-set disputes dispute-id (merge dispute { status: STATUS-CANCELED }))
    
    ;; Emit event
    (print {
      event: "dispute-canceled",
      dispute-id: dispute-id
    })
    
    (ok true)
  )
)

;; Admin functions

;; Add a member to the DAO
(define-public (add-dao-member (member principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (map-set dao-members member true)
    (print { event: "dao-member-added", member: member })
    (ok true)
  )
)

;; Remove a member from the DAO
(define-public (remove-dao-member (member principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (map-set dao-members member false)
    (print { event: "dao-member-removed", member: member })
    (ok true)
  )
)

;; Update the dispute creation fee
(define-public (update-dispute-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (var-set dispute-fee new-fee)
    (print { event: "fee-updated", new-fee: new-fee })
    (ok true)
  )
)

;; Update quorum percentage
(define-public (update-quorum-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (asserts! (and (> new-percentage u0) (<= new-percentage u100)) (err ERR-INVALID-PERCENTAGE))
    (var-set quorum-percentage new-percentage)
    (print { event: "quorum-updated", new-percentage: new-percentage })
    (ok true)
  )
)

;; Update majority percentage
(define-public (update-majority-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (asserts! (and (> new-percentage u0) (<= new-percentage u100)) (err ERR-INVALID-PERCENTAGE))
    (var-set majority-percentage new-percentage)
    (print { event: "majority-updated", new-percentage: new-percentage })
    (ok true)
  )
)

;; Withdraw contract fees to admin
(define-public (withdraw-fees)
  (let
    (
      (contract-balance (stx-get-balance (as-contract tx-sender)))
      (escrowed-funds (fold + (map-values dispute-escrow) u0))
      (fees (- contract-balance escrowed-funds))
    )
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR-NOT-AUTHORIZED))
    (asserts! (> fees u0) (err ERR-NO-FUNDS))
    
    (unwrap! (as-contract (stx-transfer? fees tx-sender (var-get admin))) (err ERR-TRANSFER-FAILED))
    
    (print { event: "fees-withdrawn", amount: fees })
    (ok fees)
  )
)