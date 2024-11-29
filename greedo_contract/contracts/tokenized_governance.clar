
;; title: tokenized_governance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Tokenized Governance Contract

;; Define the governance token
(define-fungible-token governance-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant proposal-duration u10000) ;; Number of blocks a proposal is active

;; Data variables
(define-data-var next-proposal-id uint u0)
(define-data-var interest-rate uint u500) ;; 5.00% (stored as basis points)
(define-data-var loan-term uint u30) ;; 30 days

;; Proposal structure
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 50),
    description: (string-ascii 500),
    action: (string-ascii 20),
    value: uint,
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool
  }
)

;; Vote tracking
(define-map votes
  { proposal-id: uint, voter: principal }
  { amount: uint }
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-NOT-ENDED (err u104))

;; Mint initial governance tokens to the contract owner
(ft-mint? governance-token u1000000 contract-owner)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (action (string-ascii 20)) (value uint))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (end-block (+ block-height proposal-duration))
    )
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        action: action,
        value: value,
        votes-for: u0,
        votes-against: u0,
        end-block: end-block,
        executed: false
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) (err ERR-PROPOSAL-NOT-FOUND)))
      (voter-balance (ft-get-balance governance-token tx-sender))
    )
    (asserts! (<= block-height (get end-block proposal)) (err ERR-PROPOSAL-EXPIRED))
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) (err ERR-ALREADY-VOTED))
    
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } { amount: voter-balance })
    
    (if vote-for
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) voter-balance) }))
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) voter-balance) }))
    )
    (ok true)
  )
)

;; Execute a proposal if it has passed
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) (err ERR-PROPOSAL-NOT-FOUND)))
    )
    (asserts! (>= block-height (get end-block proposal)) (err ERR-PROPOSAL-NOT-ENDED))
    (asserts! (not (get executed proposal)) (err ERR-PROPOSAL-EXPIRED))
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) (err ERR-NOT-AUTHORIZED))
    
    (if (is-eq (get action proposal) "set-interest-rate")
      (begin
        (var-set interest-rate (get value proposal))
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal { executed: true }))
        (ok true))
      (if (is-eq (get action proposal) "set-loan-term")
        (begin
          (var-set loan-term (get value proposal))
          (map-set proposals { proposal-id: proposal-id }
            (merge proposal { executed: true }))
          (ok true))
        (err ERR-NOT-AUTHORIZED)
      )
    )
  )
)

;; Getter functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-current-interest-rate)
  (ok (var-get interest-rate))
)

(define-read-only (get-current-loan-term)
  (ok (var-get loan-term))
)

;; Token transfer function (simplified for this example)
(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (ft-transfer? governance-token amount sender recipient)
)