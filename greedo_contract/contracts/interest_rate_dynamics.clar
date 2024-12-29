
;; title: interest_rate_dynamics
;; version: Interest Rate Dynamics Contract
;; summary: Allows token holders to vote on and adjust interest rates based on supply-demand metrics
;; description: Allows token holders to vote on and adjust interest rates based on supply-demand metrics

;; traits

(define-constant contract-owner tx-sender)
(define-constant min-rate u100)  ;; 1% represented as 100 basis points
(define-constant max-rate u5000) ;; 50% represented as 5000 basis points
(define-constant basis-points u10000) ;; 100% in basis points
(define-constant voting-period u144) ;; ~24 hours in blocks
(define-constant min-votes u100) ;; Minimum votes required for proposal to pass

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATE (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-PROPOSAL-EXPIRED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))

;; Data Variables
(define-data-var current-rate uint min-rate)
(define-data-var utilization-rate uint u0)
(define-data-var total-supply uint u0)
(define-data-var total-borrowed uint u0)

;; Governance Proposal Structure
(define-map rate-proposals 
    uint 
    {
        proposed-rate: uint,
        proposer: principal,
        votes-for: uint,
        votes-against: uint,
        start-block: uint,
        is-active: bool
    }
)

(define-data-var proposal-counter uint u0)

;; Read-Only Functions
(define-read-only (get-current-rate)
    (ok (var-get current-rate))
)

(define-read-only (get-utilization-rate)
    (ok (var-get utilization-rate))
)

(define-read-only (get-proposal (proposal-id uint))
    (match (map-get? rate-proposals proposal-id)
        proposal (ok proposal)
        (err ERR-PROPOSAL-NOT-FOUND)
    )
)

;; Calculate utilization
(define-private (calculate-utilization) 
    (let (
        (supply (var-get total-supply))
        (borrowed (var-get total-borrowed))
    )
    (if (is-eq supply u0)
        u0
        (/ (* borrowed basis-points) supply)
    ))
)

;; Update interest rate based on utilization
(define-public (update-utilization) 
    (begin
        (var-set utilization-rate (calculate-utilization))
        (ok (var-get utilization-rate))
    )
)

;; Create new rate proposal
(define-public (propose-rate (new-rate uint))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
    )
    (asserts! (and (>= new-rate min-rate) (<= new-rate max-rate)) ERR-INVALID-RATE)
    (map-set rate-proposals proposal-id {
        proposed-rate: new-rate,
        proposer: tx-sender,
        votes-for: u0,
        votes-against: u0,
        start-block: block-height,
        is-active: true
    })
    (var-set proposal-counter proposal-id)
    (ok proposal-id))
)

;; Vote on proposal
(define-public (vote (proposal-id uint) (vote-for bool))
    (match (map-get? rate-proposals proposal-id)
        proposal 
        (begin
            (asserts! (get is-active proposal) ERR-PROPOSAL-EXPIRED)
            (asserts! (<= (- block-height (get start-block proposal)) voting-period) ERR-PROPOSAL-EXPIRED)
            
            (if vote-for
                (map-set rate-proposals proposal-id 
                    (merge proposal { votes-for: (+ (get votes-for proposal) (stx-get-balance tx-sender)) }))
                (map-set rate-proposals proposal-id 
                    (merge proposal { votes-against: (+ (get votes-against proposal) (stx-get-balance tx-sender)) }))
            )
            (ok true))
        ERR-PROPOSAL-NOT-FOUND
    )
)

;; Finalize proposal
(define-public (finalize-proposal (proposal-id uint))
    (match (map-get? rate-proposals proposal-id)
        proposal 
        (begin
            (asserts! (>= (- block-height (get start-block proposal)) voting-period) ERR-PROPOSAL-EXPIRED)
            (asserts! (get is-active proposal) ERR-PROPOSAL-EXPIRED)
            
            (if (and 
                    (>= (+ (get votes-for proposal) (get votes-against proposal)) min-votes)
                    (> (get votes-for proposal) (get votes-against proposal))
                )
                (begin
                    (var-set current-rate (get proposed-rate proposal))
                    (map-set rate-proposals proposal-id (merge proposal { is-active: false }))
                    (ok true)
                )
                (begin
                    (map-set rate-proposals proposal-id (merge proposal { is-active: false }))
                    (ok false)
                )
            ))
        ERR-PROPOSAL-NOT-FOUND
    )
)

;; Update supply and borrow amounts
(define-public (update-pool-metrics (new-supply uint) (new-borrowed uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
        (var-set total-supply new-supply)
        (var-set total-borrowed new-borrowed)
        (ok (var-get utilization-rate))
    )
)