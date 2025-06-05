(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u101))
(define-constant ERR_SHIPMENT_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_ALREADY_DELIVERED (err u104))
(define-constant ERR_NOT_DELAYED (err u105))
(define-constant ERR_INSUFFICIENT_FUNDS (err u106))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u107))
(define-constant ERR_INVALID_PREMIUM (err u108))

(define-data-var next-shipment-id uint u1)
(define-data-var oracle-address principal CONTRACT_OWNER)
(define-data-var base-premium-rate uint u100)

(define-map shipments
  { shipment-id: uint }
  {
    shipper: principal,
    recipient: principal,
    expected-delivery: uint,
    actual-delivery: (optional uint),
    status: (string-ascii 20),
    insurance-amount: uint,
    premium-paid: uint,
    claim-processed: bool
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-map insurance-pool
  { pool-id: uint }
  { total-funds: uint }
)

(define-public (create-shipment (recipient principal) (expected-delivery uint) (insurance-amount uint))
  (let
    (
      (shipment-id (var-get next-shipment-id))
      (premium (calculate-premium insurance-amount))
      (sender-balance (get-balance tx-sender))
    )
    (asserts! (>= sender-balance premium) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> insurance-amount u0) ERR_INVALID_PREMIUM)
    (asserts! (> expected-delivery stacks-block-height) ERR_INVALID_STATUS)
    
    (map-set shipments
      { shipment-id: shipment-id }
      {
        shipper: tx-sender,
        recipient: recipient,
        expected-delivery: expected-delivery,
        actual-delivery: none,
        status: "created",
        insurance-amount: insurance-amount,
        premium-paid: premium,
        claim-processed: false
      }
    )
    
    (try! (deduct-balance tx-sender premium))
    ;; (try! (add-to-insurance-pool premium))
    (var-set next-shipment-id (+ shipment-id u1))
    
    (ok shipment-id)
  )
)

(define-public (update-shipment-status (shipment-id uint) (new-status (string-ascii 20)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get shipper shipment)) (is-eq tx-sender (var-get oracle-address))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status shipment) "delivered")) ERR_ALREADY_DELIVERED)
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { status: new-status })
    )
    
    (ok true)
  )
)

(define-public (confirm-delivery (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get recipient shipment)) (is-eq tx-sender (var-get oracle-address))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status shipment) "delivered")) ERR_ALREADY_DELIVERED)
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { 
        status: "delivered",
        actual-delivery: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (claim-insurance (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get shipper shipment)) ERR_UNAUTHORIZED)
    (asserts! (not (get claim-processed shipment)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (> current-height (get expected-delivery shipment)) ERR_NOT_DELAYED)
    (asserts! (not (is-eq (get status shipment) "delivered")) ERR_ALREADY_DELIVERED)
    
    (let
      (
        (payout-amount (calculate-payout shipment-id current-height))
      )
    ;;   (try! (add-balance (get shipper shipment) payout-amount))
      (try! (deduct-from-insurance-pool payout-amount))
      
      (map-set shipments
        { shipment-id: shipment-id }
        (merge shipment { 
          claim-processed: true,
          status: "claim-paid"
        })
      )
      
      (ok payout-amount)
    )
  )
)

(define-public (deposit-funds (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; (try! (add-balance tx-sender amount))
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (sender-balance (get-balance tx-sender))
    )
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_FUNDS)
    (try! (deduct-balance tx-sender amount))
    (try! (as-contract (stx-transfer? amount tx-sender (get shipper (unwrap! (map-get? shipments { shipment-id: u1 }) ERR_SHIPMENT_NOT_FOUND)))))
    (ok true)
  )
)

(define-public (set-oracle-address (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set oracle-address new-oracle)
    (ok true)
  )
)

(define-public (update-premium-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set base-premium-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-shipment (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (get-insurance-pool-balance)
  (default-to u0 (get total-funds (map-get? insurance-pool { pool-id: u0 })))
)

(define-read-only (calculate-premium (insurance-amount uint))
  (/ (* insurance-amount (var-get base-premium-rate)) u10000)
)

(define-read-only (calculate-payout (shipment-id uint) (current-height uint))
  (let
    (
      (shipment (unwrap-panic (map-get? shipments { shipment-id: shipment-id })))
      (delay-blocks (- current-height (get expected-delivery shipment)))
      (base-amount (get insurance-amount shipment))
    )
    (if (> delay-blocks u100)
      base-amount
      (/ (* base-amount delay-blocks) u100)
    )
  )
)

(define-read-only (is-shipment-delayed (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) (err false)))
    )
    (ok (> stacks-block-height (get expected-delivery shipment)))
  )
)

(define-read-only (get-next-shipment-id)
  (var-get next-shipment-id)
)

(define-read-only (get-oracle-address)
  (var-get oracle-address)
)

(define-private (add-balance (user principal) (amount uint))
  (let
    (
      (current-balance (get-balance user))
    )
    (map-set user-balances
      { user: user }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-private (deduct-balance (user principal) (amount uint))
  (let
    (
      (current-balance (get-balance user))
    )
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_FUNDS)
    (map-set user-balances
      { user: user }
      { balance: (- current-balance amount) }
    )
    (ok true)
  )
)

(define-private (add-to-insurance-pool (amount uint))
  (let
    (
      (current-pool (get-insurance-pool-balance))
    )
    (map-set insurance-pool
      { pool-id: u0 }
      { total-funds: (+ current-pool amount) }
    )
    (ok true)
  )
)

(define-private (deduct-from-insurance-pool (amount uint))
  (let
    (
      (current-pool (get-insurance-pool-balance))
    )
    (asserts! (>= current-pool amount) ERR_INSUFFICIENT_FUNDS)
    (map-set insurance-pool
      { pool-id: u0 }
      { total-funds: (- current-pool amount) }
    )
    (ok true)
  )
)