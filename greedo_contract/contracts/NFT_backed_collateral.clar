
;; title: NFT_backed_collateral
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

;; NFT-Backed Collateral Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-owner (err u100))
(define-constant err-already-collateralized (err u101))
(define-constant err-no-active-loan (err u102))
(define-constant err-insufficient-repayment (err u103))

;; Define data variables
(define-data-var next-loan-id uint u0)

;; Define data maps
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    nft-id: uint,
    loan-amount: uint,
    repayment-amount: uint,
    due-height: uint
  }
)

(define-map collateralized-nfts
  { nft-id: uint }
  { loan-id: uint }
)

;; NFT definition (assuming a simple NFT for demonstration)
(define-non-fungible-token collateral-nft uint)

;; Function to deposit NFT as collateral and take a loan
(define-public (deposit-collateral-and-borrow (nft-id uint) (loan-amount uint) (repayment-amount uint) (duration uint))
  (let
    (
      (loan-id (var-get next-loan-id))
      (borrower tx-sender)
    )
    (asserts! (is-none (map-get? collateralized-nfts { nft-id: nft-id })) err-already-collateralized)
    (try! (nft-transfer? collateral-nft nft-id borrower (as-contract tx-sender)))
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: borrower,
        nft-id: nft-id,
        loan-amount: loan-amount,
        repayment-amount: repayment-amount,
        due-height: (+ block-height duration)
      }
    )
    (map-set collateralized-nfts { nft-id: nft-id } { loan-id: loan-id })
    (var-set next-loan-id (+ loan-id u1))
    (as-contract (try! (stx-transfer? loan-amount tx-sender borrower)))
    (ok loan-id)
  )
)

;; Function to repay loan and retrieve collateral
(define-public (repay-loan-and-retrieve-collateral (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-no-active-loan))
      (borrower (get borrower loan))
      (nft-id (get nft-id loan))
      (repayment-amount (get repayment-amount loan))
    )
    (asserts! (is-eq tx-sender borrower) err-not-owner)
    (try! (stx-transfer? repayment-amount tx-sender (as-contract tx-sender)))
    (try! (as-contract (nft-transfer? collateral-nft nft-id tx-sender borrower)))
    (map-delete loans { loan-id: loan-id })
    (map-delete collateralized-nfts { nft-id: nft-id })
    (ok true)
  )
)

;; Function to liquidate overdue loans (can only be called by contract owner)
(define-public (liquidate-overdue-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-no-active-loan))
      (nft-id (get nft-id loan))
      (due-height (get due-height loan))
    )
    (asserts! (is-eq tx-sender contract-owner) err-not-owner)
    (asserts! (> block-height due-height) (err u104)) ;; Loan is not overdue
    (map-delete loans { loan-id: loan-id })
    (map-delete collateralized-nfts { nft-id: nft-id })
    (as-contract (try! (nft-transfer? collateral-nft nft-id tx-sender contract-owner)))
    (ok true)
  )
)

;; Read-only function to get loan details
(define-read-only (get-loan-details (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

;; Read-only function to check if an NFT is collateralized
(define-read-only (is-nft-collateralized (nft-id uint))
  (is-some (map-get? collateralized-nfts { nft-id: nft-id }))
)