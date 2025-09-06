;; ShipmentRatings Contract
;; Provides rating and feedback system for shipment experiences
;; Tracks reputation scores for shippers and recipients

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u301))
(define-constant ERR_INVALID_RATING (err u302))
(define-constant ERR_ALREADY_RATED (err u303))
(define-constant ERR_SHIPMENT_NOT_DELIVERED (err u304))
(define-constant ERR_SELF_RATING (err u305))

(define-data-var rating-counter uint u0)

;; Rating storage
(define-map shipment-ratings
  { shipment-id: uint, rater: principal }
  {
    rating: uint,
    feedback: (optional (string-ascii 200)),
    rated-at: uint,
    rating-id: uint
  }
)

;; User reputation tracking
(define-map user-reputation
  { user: principal }
  {
    total-ratings: uint,
    rating-sum: uint,
    average-rating: uint,
    last-updated: uint
  }
)

;; Rating statistics per shipment
(define-map shipment-rating-stats
  { shipment-id: uint }
  {
    total-ratings: uint,
    average-rating: uint,
    shipper-rating: (optional uint),
    recipient-rating: (optional uint)
  }
)

;; Submit rating for a shipment
(define-public (rate-shipment (shipment-id uint) (rating uint) (feedback (optional (string-ascii 200))))
  (let (
    (rater tx-sender)
    (rating-id (+ (var-get rating-counter) u1))
  )
    ;; Validate rating range
    (asserts! (and (<= rating u5) (>= rating u1)) ERR_INVALID_RATING)
    
    ;; Check if already rated by this user
    (asserts! (is-none (map-get? shipment-ratings { shipment-id: shipment-id, rater: rater })) ERR_ALREADY_RATED)
    
    ;; Store the rating
    (map-set shipment-ratings { shipment-id: shipment-id, rater: rater } {
      rating: rating,
      feedback: feedback,
      rated-at: stacks-block-height,
      rating-id: rating-id
    })
    
    ;; Update rating statistics
    (update-shipment-stats shipment-id)
    
    ;; Update user reputation (for the other party based on who is rating)
    (update-user-reputation-from-rating shipment-id rater rating)
    
    (var-set rating-counter rating-id)
    (ok rating-id)
  )
)

;; Get rating for a specific shipment by a specific rater
(define-read-only (get-shipment-rating (shipment-id uint) (rater principal))
  (map-get? shipment-ratings { shipment-id: shipment-id, rater: rater })
)

;; Get rating statistics for a shipment
(define-read-only (get-shipment-rating-stats (shipment-id uint))
  (map-get? shipment-rating-stats { shipment-id: shipment-id })
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

;; Calculate user average rating
(define-read-only (get-user-average-rating (user principal))
  (match (map-get? user-reputation { user: user })
    rep (ok (get average-rating rep))
    (ok u0)
  )
)

;; Check if user can rate a shipment
(define-read-only (can-rate-shipment (shipment-id uint) (potential-rater principal))
  (let (
    ;; This would need integration with main Shipguard contract to validate parties
    ;; For now, we'll allow any principal to rate but check for duplicates
    (existing-rating (map-get? shipment-ratings { shipment-id: shipment-id, rater: potential-rater }))
  )
    (ok (is-none existing-rating))
  )
)

;; Get total ratings count
(define-read-only (get-total-ratings-count)
  (var-get rating-counter)
)

;; Private helper functions

(define-private (update-shipment-stats (shipment-id uint))
  ;; Count ratings for this shipment (simplified approach)
  (let (
    (current-stats (default-to { total-ratings: u0, average-rating: u0, shipper-rating: none, recipient-rating: none }
                                (map-get? shipment-rating-stats { shipment-id: shipment-id })))
    (new-total (+ (get total-ratings current-stats) u1))
  )
    ;; This is a simplified update - in a full implementation we'd calculate the actual average
    (map-set shipment-rating-stats { shipment-id: shipment-id } (merge current-stats {
      total-ratings: new-total
    }))
    true
  )
)

(define-private (update-user-reputation-from-rating (shipment-id uint) (rater principal) (rating uint))
  (let (
    ;; This would determine who is being rated based on shipment parties
    ;; For simplicity, we'll update a generic reputation
    (target-user rater) ;; This would be determined from shipment data
    (current-rep (default-to { total-ratings: u0, rating-sum: u0, average-rating: u0, last-updated: u0 }
                             (map-get? user-reputation { user: target-user })))
    (new-total (+ (get total-ratings current-rep) u1))
    (new-sum (+ (get rating-sum current-rep) rating))
    (new-average (/ new-sum new-total))
  )
    (map-set user-reputation { user: target-user } {
      total-ratings: new-total,
      rating-sum: new-sum,
      average-rating: new-average,
      last-updated: stacks-block-height
    })
    true
  )
)

;; Get top-rated users (simplified)
(define-read-only (is-highly-rated-user (user principal))
  (let (
    (user-rep (get-user-reputation user))
  )
    (match user-rep
      rep (and (>= (get average-rating rep) u4) (>= (get total-ratings rep) u5))
      false
    )
  )
)

;; Calculate platform rating statistics
(define-read-only (calculate-platform-rating-average)
  ;; This is a simplified calculation - would need to iterate through all users in practice
  (ok u0) ;; Placeholder for platform average
)

;; Export user rating history count
(define-read-only (get-user-rating-count (user principal))
  (default-to u0 (get total-ratings (map-get? user-reputation { user: user })))
)
