;; carbon-tracker.clar
;; Carbon footprint tracking and route optimization for sustainable shipping
;; Enables environmental impact monitoring and eco-friendly shipping incentives

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u401))
(define-constant ERR_ROUTE_NOT_FOUND (err u402))
(define-constant ERR_INVALID_EMISSIONS (err u403))
(define-constant ERR_OFFSET_NOT_FOUND (err u404))
(define-constant ERR_INSUFFICIENT_OFFSET_BALANCE (err u405))
(define-constant ERR_INVALID_TRANSPORT_MODE (err u406))
(define-constant ERR_ROUTE_ALREADY_EXISTS (err u407))

;; Data variables
(define-data-var next-route-id uint u1)
(define-data-var total-carbon-tracked uint u0)
(define-data-var total-offsets-purchased uint u0)
(define-data-var carbon-offset-price uint u10) ;; STX per kg CO2
(define-data-var eco-reward-rate uint u5) ;; 5% discount for eco-friendly shipping

;; Transport mode emissions (kg CO2 per km per kg)
(define-map transport-emissions
  { transport-mode: (string-ascii 20) }
  { 
    emission-factor: uint, ;; CO2 grams per km per kg
    efficiency-rating: uint, ;; 1-100 scale
    sustainability-score: uint ;; 1-100 scale
  })

;; Shipping routes with carbon data
(define-map shipping-routes
  { route-id: uint }
  {
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    distance-km: uint,
    transport-mode: (string-ascii 20),
    estimated-emissions: uint,
    alternative-routes: (list 3 uint),
    optimization-score: uint,
    created-by: principal,
    usage-count: uint
  })

;; Shipment carbon tracking
(define-map shipment-carbon
  { shipment-id: uint }
  {
    route-id: uint,
    package-weight: uint,
    total-emissions: uint,
    offset-purchased: uint,
    net-emissions: uint,
    eco-score: uint,
    transport-mode: (string-ascii 20)
  })

;; Carbon offset purchases
(define-map carbon-offsets
  { user: principal, offset-id: uint }
  {
    amount-kg: uint,
    cost-paid: uint,
    purchased-at: uint,
    allocated-shipments: (list 10 uint),
    remaining-balance: uint
  })

;; User eco-statistics
(define-map user-eco-stats
  { user: principal }
  {
    total-shipments: uint,
    total-emissions: uint,
    total-offsets: uint,
    eco-score: uint,
    green-shipping-count: uint,
    carbon-neutral-count: uint
  })

;; Offset counter per user
(define-map user-offset-counter
  { user: principal }
  { counter: uint })

;; Public functions

;; Initialize transport emission factors
(define-public (setup-transport-emissions)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set transport-emissions { transport-mode: "truck" } 
      { emission-factor: u120, efficiency-rating: u65, sustainability-score: u60 })
    (map-set transport-emissions { transport-mode: "ship" }
      { emission-factor: u15, efficiency-rating: u90, sustainability-score: u85 })
    (map-set transport-emissions { transport-mode: "train" }
      { emission-factor: u25, efficiency-rating: u85, sustainability-score: u90 })
    (map-set transport-emissions { transport-mode: "plane" }
      { emission-factor: u500, efficiency-rating: u50, sustainability-score: u30 })
    (ok true)
  ))

;; Create optimized shipping route
(define-public (create-shipping-route 
    (origin (string-ascii 50)) 
    (destination (string-ascii 50)) 
    (distance-km uint) 
    (transport-mode (string-ascii 20)))
  (let (
    (route-id (var-get next-route-id))
    (emissions-data (unwrap! (map-get? transport-emissions { transport-mode: transport-mode }) ERR_INVALID_TRANSPORT_MODE))
    (estimated-emissions (/ (* distance-km (get emission-factor emissions-data)) u1000))
    (optimization-score (calculate-route-optimization-score distance-km transport-mode))
  )
    (map-set shipping-routes { route-id: route-id }
      {
        origin: origin,
        destination: destination,
        distance-km: distance-km,
        transport-mode: transport-mode,
        estimated-emissions: estimated-emissions,
        alternative-routes: (list),
        optimization-score: optimization-score,
        created-by: tx-sender,
        usage-count: u0
      })
    
    (var-set next-route-id (+ route-id u1))
    (ok route-id)
  ))

;; Track carbon footprint for shipment
(define-public (track-shipment-carbon (shipment-id uint) (route-id uint) (package-weight uint))
  (let (
    (route (unwrap! (map-get? shipping-routes { route-id: route-id }) ERR_ROUTE_NOT_FOUND))
    (total-emissions (/ (* (get estimated-emissions route) package-weight) u1000))
    (eco-score (calculate-eco-score route-id package-weight))
  )
    ;; Update route usage count
    (map-set shipping-routes { route-id: route-id }
      (merge route { usage-count: (+ (get usage-count route) u1) }))
    
    ;; Track shipment carbon data
    (map-set shipment-carbon { shipment-id: shipment-id }
      {
        route-id: route-id,
        package-weight: package-weight,
        total-emissions: total-emissions,
        offset-purchased: u0,
        net-emissions: total-emissions,
        eco-score: eco-score,
        transport-mode: (get transport-mode route)
      })
    
    ;; Update global tracking
    (var-set total-carbon-tracked (+ (var-get total-carbon-tracked) total-emissions))
    
    ;; Update user eco stats
    (unwrap-panic (update-user-eco-stats tx-sender total-emissions false))
    
    (ok total-emissions)
  ))

;; Purchase carbon offsets
(define-public (purchase-carbon-offset (amount-kg uint))
  (let (
    (cost (* amount-kg (var-get carbon-offset-price)))
    (user-counter (default-to { counter: u0 } (map-get? user-offset-counter { user: tx-sender })))
    (offset-id (+ (get counter user-counter) u1))
  )
    (asserts! (> amount-kg u0) ERR_INVALID_EMISSIONS)
    
    ;; Transfer payment for offset
    (try! (stx-transfer? cost tx-sender (as-contract tx-sender)))
    
    ;; Create offset record
    (map-set carbon-offsets { user: tx-sender, offset-id: offset-id }
      {
        amount-kg: amount-kg,
        cost-paid: cost,
        purchased-at: stacks-block-height,
        allocated-shipments: (list),
        remaining-balance: amount-kg
      })
    
    ;; Update user offset counter
    (map-set user-offset-counter { user: tx-sender } { counter: offset-id })
    
    ;; Update global offset tracking
    (var-set total-offsets-purchased (+ (var-get total-offsets-purchased) amount-kg))
    
    (ok offset-id)
  ))

;; Apply carbon offset to shipment
(define-public (apply-offset-to-shipment (shipment-id uint) (offset-id uint))
  (let (
    (carbon-data (unwrap! (map-get? shipment-carbon { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
    (offset (unwrap! (map-get? carbon-offsets { user: tx-sender, offset-id: offset-id }) ERR_OFFSET_NOT_FOUND))
    (emissions-to-offset (get net-emissions carbon-data))
    (available-offset (get remaining-balance offset))
  )
    (asserts! (>= available-offset emissions-to-offset) ERR_INSUFFICIENT_OFFSET_BALANCE)
    
    ;; Update shipment carbon data
    (map-set shipment-carbon { shipment-id: shipment-id }
      (merge carbon-data {
        offset-purchased: emissions-to-offset,
        net-emissions: u0,
        eco-score: u100
      }))
    
    ;; Update offset record
    (map-set carbon-offsets { user: tx-sender, offset-id: offset-id }
      (merge offset {
        remaining-balance: (- available-offset emissions-to-offset),
        allocated-shipments: (unwrap! (as-max-len? 
                               (append (get allocated-shipments offset) shipment-id) u10) 
                               ERR_INVALID_EMISSIONS)
      }))
    
    ;; Update user eco stats for carbon neutrality
    (unwrap-panic (update-user-eco-stats tx-sender u0 true))
    
    (ok true)
  ))

;; Get eco-friendly route alternatives
(define-read-only (get-route-alternatives (current-route-id uint))
  (let (
    (current-route (map-get? shipping-routes { route-id: current-route-id }))
  )
    (match current-route
      route (ok (get alternative-routes route))
      (err ERR_ROUTE_NOT_FOUND)
    )))

;; Calculate carbon savings from route optimization
(define-read-only (calculate-carbon-savings (original-route-id uint) (optimized-route-id uint) (weight uint))
  (let (
    (original-route (unwrap! (map-get? shipping-routes { route-id: original-route-id }) ERR_ROUTE_NOT_FOUND))
    (optimized-route (unwrap! (map-get? shipping-routes { route-id: optimized-route-id }) ERR_ROUTE_NOT_FOUND))
    (original-emissions (/ (* (get estimated-emissions original-route) weight) u1000))
    (optimized-emissions (/ (* (get estimated-emissions optimized-route) weight) u1000))
  )
    (ok {
      carbon-savings: (if (> original-emissions optimized-emissions) 
                        (- original-emissions optimized-emissions) u0),
      cost-savings: (* (- original-emissions optimized-emissions) (var-get carbon-offset-price)),
      efficiency-improvement: (/ (* (- original-emissions optimized-emissions) u100) original-emissions)
    })
  ))

;; Private helper functions

(define-private (calculate-route-optimization-score (distance uint) (transport-mode (string-ascii 20)))
  (let (
    (emissions-data (unwrap-panic (map-get? transport-emissions { transport-mode: transport-mode })))
    (base-score (get efficiency-rating emissions-data))
    (distance-penalty (/ distance u100))
  )
    (if (> base-score distance-penalty)
      (- base-score distance-penalty)
      u10
    )))

(define-private (calculate-eco-score (route-id uint) (weight uint))
  (let (
    (route (unwrap-panic (map-get? shipping-routes { route-id: route-id })))
    (emissions-per-kg (/ (get estimated-emissions route) u1000))
  )
    ;; Higher score for lower emissions
    (if (<= emissions-per-kg u50)
      u90
      (if (<= emissions-per-kg u100)
        u70
        (if (<= emissions-per-kg u200)
          u50
          u30)))))

(define-private (update-user-eco-stats (user principal) (emissions uint) (is-carbon-neutral bool))
  (let (
    (current-stats (default-to 
      { total-shipments: u0, total-emissions: u0, total-offsets: u0, eco-score: u50, 
        green-shipping-count: u0, carbon-neutral-count: u0 }
      (map-get? user-eco-stats { user: user })))
    (new-green-count (if (< emissions u50) (+ (get green-shipping-count current-stats) u1) 
                       (get green-shipping-count current-stats)))
    (new-neutral-count (if is-carbon-neutral (+ (get carbon-neutral-count current-stats) u1) 
                         (get carbon-neutral-count current-stats)))
    (new-total-shipments (+ (get total-shipments current-stats) u1))
    (new-eco-score (/ (+ (* new-green-count u30) (* new-neutral-count u50)) new-total-shipments))
  )
    (map-set user-eco-stats { user: user }
      {
        total-shipments: new-total-shipments,
        total-emissions: (+ (get total-emissions current-stats) emissions),
        total-offsets: (get total-offsets current-stats),
        eco-score: new-eco-score,
        green-shipping-count: new-green-count,
        carbon-neutral-count: new-neutral-count
      })
    (ok true)
  ))

;; Read-only functions

(define-read-only (get-shipment-carbon-data (shipment-id uint))
  (map-get? shipment-carbon { shipment-id: shipment-id }))

(define-read-only (get-shipping-route (route-id uint))
  (map-get? shipping-routes { route-id: route-id }))

(define-read-only (get-transport-emissions (transport-mode (string-ascii 20)))
  (map-get? transport-emissions { transport-mode: transport-mode }))

(define-read-only (get-user-carbon-offset (user principal) (offset-id uint))
  (map-get? carbon-offsets { user: user, offset-id: offset-id }))

(define-read-only (get-user-eco-statistics (user principal))
  (default-to 
    { total-shipments: u0, total-emissions: u0, total-offsets: u0, eco-score: u50, 
      green-shipping-count: u0, carbon-neutral-count: u0 }
    (map-get? user-eco-stats { user: user })))

(define-read-only (get-platform-carbon-stats)
  {
    total-tracked: (var-get total-carbon-tracked),
    total-offsets: (var-get total-offsets-purchased),
    net-emissions: (- (var-get total-carbon-tracked) (var-get total-offsets-purchased)),
    offset-price: (var-get carbon-offset-price),
    eco-reward-rate: (var-get eco-reward-rate)
  })

(define-read-only (calculate-eco-reward (emissions uint) (shipment-value uint))
  (let (
    (is-eco-friendly (<= emissions u50))
    (reward-amount (if is-eco-friendly 
                     (/ (* shipment-value (var-get eco-reward-rate)) u100) 
                     u0))
  )
    (ok {
      eligible: is-eco-friendly,
      reward-amount: reward-amount,
      emissions-level: (if (<= emissions u25) "ultra-low" 
                         (if (<= emissions u50) "low" "standard"))
    })
  ))

(define-read-only (get-route-recommendations (origin (string-ascii 50)) (destination (string-ascii 50)))
  (ok {
    eco-friendliest: "ship", ;; Simplified recommendation
    fastest: "plane",
    most-economical: "train",
    balanced: "truck",
    carbon-impact: "Consider ship transport for 85% lower emissions"
  }))

(define-read-only (is-carbon-neutral-shipment (shipment-id uint))
  (let (
    (carbon-data (map-get? shipment-carbon { shipment-id: shipment-id }))
  )
    (match carbon-data
      data (ok (<= (get net-emissions data) u0))
      (ok false)
    )))

(define-read-only (get-total-platform-impact)
  (let (
    (total-tracked (var-get total-carbon-tracked))
    (total-offset (var-get total-offsets-purchased))
  )
    (ok {
      total-emissions-kg: total-tracked,
      total-offsets-kg: total-offset,
      net-platform-emissions: (if (> total-tracked total-offset) 
                                 (- total-tracked total-offset) u0),
      carbon-neutrality-percentage: (if (> total-tracked u0)
                                      (/ (* total-offset u100) total-tracked) u0)
    })
  ))
