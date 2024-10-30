
;; title: reputation-based
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

;; Reputation-Based Scoring Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-INVALID-SCORE (err u102))

;; Data Variables
(define-map user-scores 
    principal 
    {
        score: uint,
        total-loans: uint,
        repaid-loans: uint,
        default-count: uint,
        last-update: uint
    }
)

(define-map authorized-operators principal bool)

;; Initialize contract owner
(define-data-var contract-owner principal tx-sender)

;; Public Functions

;; Initialize a new user's score
(define-public (initialize-user (user principal))
    (begin
        (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
        (ok (map-set user-scores user {
            score: u500,  ;; Initial score of 500
            total-loans: u0,
            repaid-loans: u0,
            default-count: u0,
            last-update: block-height
        }))
    )
)

;; Helper function to cap the score at 1000
(define-private (cap-score (score uint))
    (if (> score u1000)
        u1000
        score
    )
)

;; Update user score based on loan repayment
(define-public (update-score-on-repayment (user principal))
    (let (
        (current-data (unwrap! (map-get? user-scores user) ERR-USER-NOT-FOUND))
        (new-score (cap-score (+ (get score current-data) u50)))
    )
        (begin
            (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
            (ok (map-set user-scores user
                {
                    score: new-score,
                    total-loans: (+ (get total-loans current-data) u1),
                    repaid-loans: (+ (get repaid-loans current-data) u1),
                    default-count: (get default-count current-data),
                    last-update: block-height
                }
            ))
        )
    )
)

;; Helper function to ensure score doesn't go below 0
(define-private (ensure-min-score (current-score uint) (penalty uint))
    (if (>= current-score penalty)
        (- current-score penalty)
        u0
    )
)

;; Update score on loan default
(define-public (update-score-on-default (user principal))
    (let (
        (current-data (unwrap! (map-get? user-scores user) ERR-USER-NOT-FOUND))
        (new-score (ensure-min-score (get score current-data) u100))
    )
        (begin
            (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
            (ok (map-set user-scores user
                {
                    score: new-score,
                    total-loans: (+ (get total-loans current-data) u1),
                    repaid-loans: (get repaid-loans current-data),
                    default-count: (+ (get default-count current-data) u1),
                    last-update: block-height
                }
            ))
        )
    )
)

;; Get user's current score
(define-read-only (get-user-score (user principal))
    (ok (get score (unwrap! (map-get? user-scores user) ERR-USER-NOT-FOUND)))
)

;; Get user's complete profile
(define-read-only (get-user-profile (user principal))
    (ok (unwrap! (map-get? user-scores user) ERR-USER-NOT-FOUND))
)

;; Administrative Functions

;; Add authorized operator
(define-public (add-authorized-operator (operator principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-operators operator true))
    )
)

;; Remove authorized operator
(define-public (remove-authorized-operator (operator principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-operators operator false))
    )
)

;; Private Functions

;; Check if caller is authorized
(define-private (is-authorized)
    (or 
        (is-contract-owner)
        (default-to false (map-get? authorized-operators tx-sender))
    )
)

;; Check if caller is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)