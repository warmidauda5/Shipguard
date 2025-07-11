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
(define-constant ERR_INVALID_MILESTONE (err u109))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u110))
(define-constant ERR_MILESTONE_ORDER_VIOLATION (err u111))
(define-constant ERR_MILESTONE_NOT_FOUND (err u112))

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

(define-map shipment-milestones
  { shipment-id: uint, milestone-id: uint }
  {
    milestone-name: (string-ascii 30),
    completed: bool,
    completion-timestamp: (optional uint),
    completed-by: (optional principal),
    location: (optional (string-ascii 50)),
    notes: (optional (string-ascii 100))
  }
)

(define-map milestone-definitions
  { milestone-id: uint }
  {
    name: (string-ascii 30),
    order-index: uint,
    is-critical: bool,
    max-delay-blocks: uint
  }
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
    (initialize-shipment-milestones shipment-id)
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

(define-public (setup-milestone-definitions)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set milestone-definitions
      { milestone-id: u1 }
      { name: "picked-up", order-index: u1, is-critical: true, max-delay-blocks: u144 }
    )
    (map-set milestone-definitions
      { milestone-id: u2 }
      { name: "in-transit", order-index: u2, is-critical: true, max-delay-blocks: u288 }
    )
    (map-set milestone-definitions
      { milestone-id: u3 }
      { name: "customs-cleared", order-index: u3, is-critical: false, max-delay-blocks: u432 }
    )
    (map-set milestone-definitions
      { milestone-id: u4 }
      { name: "out-for-delivery", order-index: u4, is-critical: true, max-delay-blocks: u72 }
    )
    (map-set milestone-definitions
      { milestone-id: u5 }
      { name: "delivered", order-index: u5, is-critical: true, max-delay-blocks: u0 }
    )
    (ok true)
  )
)

(define-public (complete-milestone (shipment-id uint) (milestone-id uint) (location (optional (string-ascii 50))) (notes (optional (string-ascii 100))))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (milestone-def (unwrap! (map-get? milestone-definitions { milestone-id: milestone-id }) ERR_INVALID_MILESTONE))
      (current-milestone (unwrap! (map-get? shipment-milestones { shipment-id: shipment-id, milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get shipper shipment)) (is-eq tx-sender (get recipient shipment)) (is-eq tx-sender (var-get oracle-address))) ERR_UNAUTHORIZED)
    (asserts! (not (get completed current-milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (is-milestone-order-valid shipment-id milestone-id) ERR_MILESTONE_ORDER_VIOLATION)
    
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: milestone-id }
      {
        milestone-name: (get name milestone-def),
        completed: true,
        completion-timestamp: (some stacks-block-height),
        completed-by: (some tx-sender),
        location: location,
        notes: notes
      }
    )
    
    (if (is-eq milestone-id u5)
      (begin
        (try! (update-shipment-status shipment-id "delivered"))
        (try! (confirm-delivery shipment-id))
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (bulk-complete-milestones (shipment-id uint) (milestone-ids (list 5 uint)) (location (optional (string-ascii 50))) (notes (optional (string-ascii 100))))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get shipper shipment)) (is-eq tx-sender (get recipient shipment)) (is-eq tx-sender (var-get oracle-address))) ERR_UNAUTHORIZED)
    (fold complete-milestone-in-bulk milestone-ids (ok shipment-id))
  )
)

(define-public (get-shipment-progress (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (milestone-1 (get-milestone-status shipment-id u1))
      (milestone-2 (get-milestone-status shipment-id u2))
      (milestone-3 (get-milestone-status shipment-id u3))
      (milestone-4 (get-milestone-status shipment-id u4))
      (milestone-5 (get-milestone-status shipment-id u5))
    )
    (ok {
      shipment-id: shipment-id,
      picked-up: milestone-1,
      in-transit: milestone-2,
      customs-cleared: milestone-3,
      out-for-delivery: milestone-4,
      delivered: milestone-5,
      overall-progress: (calculate-progress-percentage shipment-id)
    })
  )
)

(define-public (get-delayed-milestones (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (current-height stacks-block-height)
      (delayed-list (list u0 u0 u0 u0 u0))
    )
    (ok (fold check-milestone-delay (list u1 u2 u3 u4 u5) delayed-list))
  )
)

(define-read-only (get-milestone-status (shipment-id uint) (milestone-id uint))
  (default-to
    { milestone-name: "unknown", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    (map-get? shipment-milestones { shipment-id: shipment-id, milestone-id: milestone-id })
  )
)

(define-read-only (get-milestone-definition (milestone-id uint))
  (map-get? milestone-definitions { milestone-id: milestone-id })
)

(define-read-only (calculate-progress-percentage (shipment-id uint))
  (let
    (
      (completed-count (count-completed-milestones shipment-id))
    )
    (/ (* completed-count u100) u5)
  )
)

(define-read-only (is-milestone-critical (milestone-id uint))
  (default-to false (get is-critical (map-get? milestone-definitions { milestone-id: milestone-id })))
)

(define-read-only (get-milestone-max-delay (milestone-id uint))
  (default-to u0 (get max-delay-blocks (map-get? milestone-definitions { milestone-id: milestone-id })))
)

(define-private (initialize-shipment-milestones (shipment-id uint))
  (begin
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: u1 }
      { milestone-name: "picked-up", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    )
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: u2 }
      { milestone-name: "in-transit", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    )
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: u3 }
      { milestone-name: "customs-cleared", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    )
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: u4 }
      { milestone-name: "out-for-delivery", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    )
    (map-set shipment-milestones
      { shipment-id: shipment-id, milestone-id: u5 }
      { milestone-name: "delivered", completed: false, completion-timestamp: none, completed-by: none, location: none, notes: none }
    )
    true
  )
)

(define-private (is-milestone-order-valid (shipment-id uint) (milestone-id uint))
  (let
    (
      (target-order (default-to u0 (get order-index (map-get? milestone-definitions { milestone-id: milestone-id }))))
    )
    (if (is-eq target-order u1)
      true
      (let
        (
          (previous-milestone-id (- milestone-id u1))
          (previous-milestone (get-milestone-status shipment-id previous-milestone-id))
        )
        (get completed previous-milestone)
      )
    )
  )
)

(define-private (count-completed-milestones (shipment-id uint))
  (let
    (
      (m1 (get completed (get-milestone-status shipment-id u1)))
      (m2 (get completed (get-milestone-status shipment-id u2)))
      (m3 (get completed (get-milestone-status shipment-id u3)))
      (m4 (get completed (get-milestone-status shipment-id u4)))
      (m5 (get completed (get-milestone-status shipment-id u5)))
    )
    (+ (if m1 u1 u0) (+ (if m2 u1 u0) (+ (if m3 u1 u0) (+ (if m4 u1 u0) (if m5 u1 u0)))))
  )
)

(define-private (complete-milestone-in-bulk (milestone-id uint) (result (response uint uint)))
  (match result
    success (let
      (
        (shipment-id success)
        (milestone-exists (is-some (map-get? shipment-milestones { shipment-id: shipment-id, milestone-id: milestone-id })))
        (milestone-completed (get completed (get-milestone-status shipment-id milestone-id)))
      )
      (if (and milestone-exists (not milestone-completed))
        (begin
          (map-set shipment-milestones
            { shipment-id: shipment-id, milestone-id: milestone-id }
            {
              milestone-name: (default-to "unknown" (get name (map-get? milestone-definitions { milestone-id: milestone-id }))),
              completed: true,
              completion-timestamp: (some stacks-block-height),
              completed-by: (some tx-sender),
              location: none,
              notes: none
            }
          )
          (ok shipment-id)
        )
        (ok shipment-id)
      )
    )
    error (err error)
  )
)

(define-private (check-milestone-delay (milestone-id uint) (acc (list 5 uint)))
  (let
    (
      (current-height stacks-block-height)
      (milestone-def (map-get? milestone-definitions { milestone-id: milestone-id }))
      (max-delay (default-to u0 (get max-delay-blocks milestone-def)))
    )
    (if (> max-delay u0)
      acc
      acc
    )
  )
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