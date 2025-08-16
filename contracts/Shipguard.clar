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
(define-constant ERR_ESCROW_NOT_FOUND (err u113))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u114))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u115))
(define-constant ERR_INVALID_RELEASE_AMOUNT (err u116))
(define-constant ERR_NOT_ESCROW_PARTY (err u117))
(define-constant ERR_DISPUTE_ACTIVE (err u118))
(define-constant ERR_DISPUTE_NOT_FOUND (err u119))
(define-constant ERR_INVALID_VOTE (err u120))

(define-data-var next-shipment-id uint u1)
(define-data-var oracle-address principal CONTRACT_OWNER)
(define-data-var base-premium-rate uint u100)
(define-data-var next-escrow-id uint u1)
(define-data-var dispute-resolution-period uint u1440)

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

(define-map escrow-accounts
  { escrow-id: uint }
  {
    shipment-id: uint,
    payer: principal,
    payee: principal,
    amount: uint,
    released-amount: uint,
    created-at: uint,
    release-deadline: uint,
    status: (string-ascii 20),
    payer-approved: bool,
    payee-approved: bool,
    oracle-approved: bool,
    dispute-active: bool
  }
)

(define-map escrow-disputes
  { escrow-id: uint }
  {
    initiated-by: principal,
    initiated-at: uint,
    reason: (string-ascii 200),
    evidence-hash: (optional (string-ascii 64)),
    resolution-deadline: uint,
    resolved: bool,
    resolution: (optional (string-ascii 100)),
    votes-for-payer: uint,
    votes-for-payee: uint,
    total-votes: uint
  }
)

(define-map dispute-votes
  { escrow-id: uint, voter: principal }
  { vote: (string-ascii 10), voted-at: uint }
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

(define-public (create-escrow (shipment-id uint) (payee principal) (amount uint) (release-deadline uint))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-height stacks-block-height)
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get shipper shipment)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_RELEASE_AMOUNT)
    (asserts! (> release-deadline current-height) ERR_INVALID_STATUS)
    (asserts! (is-none (map-get? escrow-accounts { escrow-id: escrow-id })) ERR_ESCROW_ALREADY_EXISTS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-accounts
      { escrow-id: escrow-id }
      {
        shipment-id: shipment-id,
        payer: tx-sender,
        payee: payee,
        amount: amount,
        released-amount: u0,
        created-at: current-height,
        release-deadline: release-deadline,
        status: "active",
        payer-approved: false,
        payee-approved: false,
        oracle-approved: false,
        dispute-active: false
      }
    )
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (approve-escrow-release (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-accounts { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (not (get dispute-active escrow)) ERR_DISPUTE_ACTIVE)
    (asserts! (< current-height (get release-deadline escrow)) ERR_INVALID_STATUS)
    (asserts! (is-eq (get status escrow) "active") ERR_ESCROW_ALREADY_RELEASED)
    
    (let
      (
        (updated-escrow
          (if (is-eq tx-sender (get payer escrow))
            (merge escrow { payer-approved: true })
            (if (is-eq tx-sender (get payee escrow))
              (merge escrow { payee-approved: true })
              (if (is-eq tx-sender (var-get oracle-address))
                (merge escrow { oracle-approved: true })
                escrow
              )
            )
          )
        )
      )
      (asserts! (or (is-eq tx-sender (get payer escrow)) (is-eq tx-sender (get payee escrow)) (is-eq tx-sender (var-get oracle-address))) ERR_NOT_ESCROW_PARTY)
      
      (map-set escrow-accounts { escrow-id: escrow-id } updated-escrow)
      
      (let
        (
          (approval-count (+ (if (get payer-approved updated-escrow) u1 u0) (+ (if (get payee-approved updated-escrow) u1 u0) (if (get oracle-approved updated-escrow) u1 u0))))
        )
        (if (>= approval-count u2)
          (execute-escrow-release escrow-id)
          (ok true)
        )
      )
    )
  )
)

(define-public (release-escrow-partial (escrow-id uint) (release-amount uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-accounts { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (var-get oracle-address)) ERR_UNAUTHORIZED)
    (asserts! (not (get dispute-active escrow)) ERR_DISPUTE_ACTIVE)
    (asserts! (is-eq (get status escrow) "active") ERR_ESCROW_ALREADY_RELEASED)
    (asserts! (<= release-amount (- (get amount escrow) (get released-amount escrow))) ERR_INVALID_RELEASE_AMOUNT)
    
    (try! (as-contract (stx-transfer? release-amount tx-sender (get payee escrow))))
    
    (let
      (
        (new-released-amount (+ (get released-amount escrow) release-amount))
        (new-status (if (is-eq new-released-amount (get amount escrow)) "completed" "active"))
      )
      (map-set escrow-accounts
        { escrow-id: escrow-id }
        (merge escrow { 
          released-amount: new-released-amount,
          status: new-status
        })
      )
    )
    
    (ok release-amount)
  )
)

(define-public (initiate-escrow-dispute (escrow-id uint) (reason (string-ascii 200)) (evidence-hash (optional (string-ascii 64))))
  (let
    (
      (escrow (unwrap! (map-get? escrow-accounts { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get payer escrow)) (is-eq tx-sender (get payee escrow))) ERR_NOT_ESCROW_PARTY)
    (asserts! (not (get dispute-active escrow)) ERR_DISPUTE_ACTIVE)
    (asserts! (is-eq (get status escrow) "active") ERR_ESCROW_ALREADY_RELEASED)
    
    (map-set escrow-disputes
      { escrow-id: escrow-id }
      {
        initiated-by: tx-sender,
        initiated-at: current-height,
        reason: reason,
        evidence-hash: evidence-hash,
        resolution-deadline: (+ current-height (var-get dispute-resolution-period)),
        resolved: false,
        resolution: none,
        votes-for-payer: u0,
        votes-for-payee: u0,
        total-votes: u0
      }
    )
    
    (map-set escrow-accounts
      { escrow-id: escrow-id }
      (merge escrow { dispute-active: true })
    )
    
    (ok true)
  )
)

(define-public (vote-on-dispute (escrow-id uint) (vote (string-ascii 10)))
  (let
    (
      (dispute (unwrap! (map-get? escrow-disputes { escrow-id: escrow-id }) ERR_DISPUTE_NOT_FOUND))
      (current-height stacks-block-height)
      (existing-vote (map-get? dispute-votes { escrow-id: escrow-id, voter: tx-sender }))
    )
    (asserts! (not (get resolved dispute)) ERR_DISPUTE_NOT_FOUND)
    (asserts! (< current-height (get resolution-deadline dispute)) ERR_INVALID_STATUS)
    (asserts! (or (is-eq vote "payer") (is-eq vote "payee")) ERR_INVALID_VOTE)
    (asserts! (is-none existing-vote) ERR_INVALID_VOTE)
    
    (map-set dispute-votes
      { escrow-id: escrow-id, voter: tx-sender }
      { vote: vote, voted-at: current-height }
    )
    
    (let
      (
        (new-votes-for-payer (if (is-eq vote "payer") (+ (get votes-for-payer dispute) u1) (get votes-for-payer dispute)))
        (new-votes-for-payee (if (is-eq vote "payee") (+ (get votes-for-payee dispute) u1) (get votes-for-payee dispute)))
        (new-total-votes (+ (get total-votes dispute) u1))
      )
      (map-set escrow-disputes
        { escrow-id: escrow-id }
        (merge dispute {
          votes-for-payer: new-votes-for-payer,
          votes-for-payee: new-votes-for-payee,
          total-votes: new-total-votes
        })
      )
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (escrow-id uint))
  (let
    (
      (dispute (unwrap! (map-get? escrow-disputes { escrow-id: escrow-id }) ERR_DISPUTE_NOT_FOUND))
      (escrow (unwrap! (map-get? escrow-accounts { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (or (>= current-height (get resolution-deadline dispute)) (is-eq tx-sender (var-get oracle-address))) ERR_UNAUTHORIZED)
    (asserts! (not (get resolved dispute)) ERR_DISPUTE_NOT_FOUND)
    (asserts! (get dispute-active escrow) ERR_DISPUTE_NOT_FOUND)
    
    (let
      (
        (payer-votes (get votes-for-payer dispute))
        (payee-votes (get votes-for-payee dispute))
        (winner (if (> payer-votes payee-votes) "payer" "payee"))
        (refund-to (if (is-eq winner "payer") (get payer escrow) (get payee escrow)))
        (remaining-amount (- (get amount escrow) (get released-amount escrow)))
      )
      (try! (as-contract (stx-transfer? remaining-amount tx-sender refund-to)))
      
      (map-set escrow-disputes
        { escrow-id: escrow-id }
        (merge dispute {
          resolved: true,
          resolution: (some (if (is-eq winner "payer") "refunded-to-payer" "released-to-payee"))
        })
      )
      
      (map-set escrow-accounts
        { escrow-id: escrow-id }
        (merge escrow {
          status: "disputed-resolved",
          dispute-active: false,
          released-amount: (get amount escrow)
        })
      )
      
      (ok winner)
    )
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-accounts { escrow-id: escrow-id })
)

(define-read-only (get-escrow-dispute (escrow-id uint))
  (map-get? escrow-disputes { escrow-id: escrow-id })
)

(define-read-only (get-dispute-vote (escrow-id uint) (voter principal))
  (map-get? dispute-votes { escrow-id: escrow-id, voter: voter })
)

(define-read-only (calculate-escrow-status (escrow-id uint))
  (let
    (
      (escrow (map-get? escrow-accounts { escrow-id: escrow-id }))
      (current-height stacks-block-height)
    )
    (match escrow
      account (let
        (
          (is-expired (> current-height (get release-deadline account)))
          (is-fully-released (is-eq (get released-amount account) (get amount account)))
          (approval-count (+ (if (get payer-approved account) u1 u0) (+ (if (get payee-approved account) u1 u0) (if (get oracle-approved account) u1 u0))))
        )
        (ok {
          status: (get status account),
          is-expired: is-expired,
          is-fully-released: is-fully-released,
          approval-count: approval-count,
          ready-for-release: (and (>= approval-count u2) (not (get dispute-active account)))
        })
      )
      (err ERR_ESCROW_NOT_FOUND)
    )
  )
)

(define-private (execute-escrow-release (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-accounts { escrow-id: escrow-id }) ERR_ESCROW_NOT_FOUND))
      (remaining-amount (- (get amount escrow) (get released-amount escrow)))
    )
    (try! (as-contract (stx-transfer? remaining-amount tx-sender (get payee escrow))))
    
    (map-set escrow-accounts
      { escrow-id: escrow-id }
      (merge escrow { 
        released-amount: (get amount escrow),
        status: "completed"
      })
    )
    
    (ok true)
  )
)

