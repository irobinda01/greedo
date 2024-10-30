
;; title: loan_contract
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

;; loan-contract

;; Constants
(define-constant ERR-INVALID-AMOUNT (err u1))
(define-constant ERR-INVALID-DURATION (err u2))
(define-constant ERR-LOAN-ACTIVE (err u3))
(define-constant ERR-UNAUTHORIZED (err u4))
(define-constant ERR-INSUFFICIENT-FUNDS (err u5))
(define-constant ERR-LOAN-NOT-ACTIVE (err u6))
(define-constant ERR-ALREADY-REPAID (err u7))

;; Data Variables
(define-data-var loan-counter uint u0)

;; Define loan structure using a map
(define-map loans
    uint
    {
        amount: uint,
        interest-rate: uint,
        duration: uint,
        start-time: uint,
        borrower: principal,
        lender: (optional principal),
        active: bool,
        repaid: bool
    }
)

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
    (map-get? loans loan-id)
)

(define-read-only (calculate-repayment-amount (loan-id uint))
    (match (map-get? loans loan-id)
        loan 
        (ok (let (
            (interest-amount (/ (* (* (get amount loan) (get interest-rate loan)) (get duration loan)) (* u365 u100)))
        )
        (+ (get amount loan) interest-amount)))
        ERR-INVALID-AMOUNT
    )
)

;; Public functions
(define-public (create-loan (amount uint) (interest-rate uint) (duration uint))
    (let (
        (loan-id (var-get loan-counter))
    )
    (begin
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration u0) ERR-INVALID-DURATION)
        
        (map-set loans loan-id {
            amount: amount,
            interest-rate: interest-rate,
            duration: duration,
            start-time: u0,
            borrower: tx-sender,
            lender: none,
            active: false,
            repaid: false
        })
        
        (var-set loan-counter (+ loan-id u1))
        (ok loan-id)
    ))
)

(define-public (fund-loan (loan-id uint))
    (let (
        (loan (unwrap! (map-get? loans loan-id) ERR-INVALID-AMOUNT))
    )
    (begin
        (asserts! (not (get active loan)) ERR-LOAN-ACTIVE)
        (asserts! (is-eq (stx-get-balance tx-sender) (get amount loan)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer STX from lender to borrower
        (try! (stx-transfer? (get amount loan) tx-sender (get borrower loan)))
        
        ;; Update loan status
        (map-set loans loan-id (merge loan {
            lender: (some tx-sender),
            active: true,
            start-time: block-height
        }))
        
        (ok true)
    ))
)

(define-public (repay-loan (loan-id uint))
    (let (
        (loan (unwrap! (map-get? loans loan-id) ERR-INVALID-AMOUNT))
        (repayment-amount (unwrap! (calculate-repayment-amount loan-id) ERR-INVALID-AMOUNT))
    )
    (begin
        (asserts! (get active loan) ERR-LOAN-NOT-ACTIVE)
        (asserts! (not (get repaid loan)) ERR-ALREADY-REPAID)
        (asserts! (is-eq tx-sender (get borrower loan)) ERR-UNAUTHORIZED)
        
        ;; Transfer STX from borrower to lender
        (try! (stx-transfer? repayment-amount tx-sender (unwrap! (get lender loan) ERR-UNAUTHORIZED)))
        
        ;; Update loan status
        (map-set loans loan-id (merge loan {
            active: false,
            repaid: true
        }))
        
        (ok true)
    ))
)

;; Check if loan is defaulted
(define-read-only (is-loan-defaulted (loan-id uint))
    (match (map-get? loans loan-id)
        loan
        (and
            (get active loan)
            (not (get repaid loan))
            (> block-height (+ (get start-time loan) (get duration loan)))
        )
        false ;; If the loan doesn't exist, return false
    )
)