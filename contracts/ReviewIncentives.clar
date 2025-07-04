(define-fungible-token review-token)

(define-map reviewer-stats
  { reviewer: principal }
  {
    reviews-completed: uint,
    total-tokens-earned: uint,
    average-review-quality: uint,
    tokens-spent: uint
  }
)

(define-map review-rewards
  { paper-id: uint, reviewer: principal }
  {
    base-reward: uint,
    quality-bonus: uint,
    claimed: bool
  }
)

(define-map review-quality-votes
  { paper-id: uint, reviewer: principal, voter: principal }
  {
    quality-score: uint,
    timestamp: uint
  }
)

(define-map fast-track-requests
  { paper-id: uint }
  {
    requester: principal,
    tokens-paid: uint,
    priority-level: uint,
    timestamp: uint
  }
)

(define-data-var base-review-reward uint u100)
(define-data-var fast-track-cost uint u500)
(define-data-var quality-vote-threshold uint u3)

(define-constant ERR_INSUFFICIENT_TOKENS u10)
(define-constant ERR_ALREADY_CLAIMED u11)
(define-constant ERR_INVALID_QUALITY_SCORE u12)
(define-constant ERR_CANNOT_VOTE_OWN_REVIEW u13)
(define-constant ERR_REVIEW_NOT_FOUND u14)

(define-public (mint-initial-tokens (amount uint))
  (ft-mint? review-token amount tx-sender)
)

(define-public (claim-review-reward (paper-id uint))
  (let
    (
      (reward-info (unwrap! (map-get? review-rewards { paper-id: paper-id, reviewer: tx-sender }) (err ERR_REVIEW_NOT_FOUND)))
      (total-reward (+ (get base-reward reward-info) (get quality-bonus reward-info)))
      (current-stats (default-to 
        { reviews-completed: u0, total-tokens-earned: u0, average-review-quality: u0, tokens-spent: u0 }
        (map-get? reviewer-stats { reviewer: tx-sender })))
    )
    (asserts! (not (get claimed reward-info)) (err ERR_ALREADY_CLAIMED))
    
    (try! (ft-mint? review-token total-reward tx-sender))
    
    (map-set review-rewards
      { paper-id: paper-id, reviewer: tx-sender }
      (merge reward-info { claimed: true })
    )
    
    (map-set reviewer-stats
      { reviewer: tx-sender }
      {
        reviews-completed: (+ (get reviews-completed current-stats) u1),
        total-tokens-earned: (+ (get total-tokens-earned current-stats) total-reward),
        average-review-quality: (get average-review-quality current-stats),
        tokens-spent: (get tokens-spent current-stats)
      }
    )
    
    (ok total-reward)
  )
)

(define-public (set-review-reward (paper-id uint) (reviewer principal))
  (let
    (
      (base-reward (var-get base-review-reward))
    )
    (ok (map-set review-rewards
      { paper-id: paper-id, reviewer: reviewer }
      {
        base-reward: base-reward,
        quality-bonus: u0,
        claimed: false
      }
    ))
  )
)

(define-public (vote-review-quality (paper-id uint) (reviewer principal) (quality-score uint))
  (begin
    (asserts! (not (is-eq tx-sender reviewer)) (err ERR_CANNOT_VOTE_OWN_REVIEW))
    (asserts! (and (>= quality-score u1) (<= quality-score u10)) (err ERR_INVALID_QUALITY_SCORE))
    
    (map-set review-quality-votes
      { paper-id: paper-id, reviewer: reviewer, voter: tx-sender }
      {
        quality-score: quality-score,
        timestamp: stacks-block-height
      }
    )
    
    (try! (update-quality-bonus paper-id reviewer))
    (ok true)
  )
)

(define-private (update-quality-bonus (paper-id uint) (reviewer principal))
  (let
    (
      (current-reward (unwrap! (map-get? review-rewards { paper-id: paper-id, reviewer: reviewer }) (err ERR_REVIEW_NOT_FOUND)))
      (quality-bonus (calculate-quality-bonus paper-id reviewer))
    )
    (ok (map-set review-rewards
      { paper-id: paper-id, reviewer: reviewer }
      (merge current-reward { quality-bonus: quality-bonus })
    ))
  )
)

(define-private (calculate-quality-bonus (paper-id uint) (reviewer principal))
  (let
    (
      (base-bonus u50)
    )
    base-bonus
  )
)

(define-public (request-fast-track-review (paper-id uint) (priority-level uint))
  (let
    (
      (cost (* (var-get fast-track-cost) priority-level))
      (current-balance (ft-get-balance review-token tx-sender))
      (current-stats (default-to 
        { reviews-completed: u0, total-tokens-earned: u0, average-review-quality: u0, tokens-spent: u0 }
        (map-get? reviewer-stats { reviewer: tx-sender })))
    )
    (asserts! (>= current-balance cost) (err ERR_INSUFFICIENT_TOKENS))
    (asserts! (and (>= priority-level u1) (<= priority-level u5)) (err ERR_INVALID_QUALITY_SCORE))
    
    (try! (ft-burn? review-token cost tx-sender))
    
    (map-set fast-track-requests
      { paper-id: paper-id }
      {
        requester: tx-sender,
        tokens-paid: cost,
        priority-level: priority-level,
        timestamp: stacks-block-height
      }
    )
    
    (map-set reviewer-stats
      { reviewer: tx-sender }
      (merge current-stats { tokens-spent: (+ (get tokens-spent current-stats) cost) })
    )
    
    (ok true)
  )
)

(define-public (transfer-tokens (recipient principal) (amount uint))
  (ft-transfer? review-token amount tx-sender recipient)
)

(define-read-only (get-token-balance (account principal))
  (ft-get-balance review-token account)
)

(define-read-only (get-reviewer-stats (reviewer principal))
  (map-get? reviewer-stats { reviewer: reviewer })
)

(define-read-only (get-review-reward-info (paper-id uint) (reviewer principal))
  (map-get? review-rewards { paper-id: paper-id, reviewer: reviewer })
)

(define-read-only (get-fast-track-info (paper-id uint))
  (map-get? fast-track-requests { paper-id: paper-id })
)

(define-read-only (get-total-supply)
  (ft-get-supply review-token)
)

(define-public (set-base-reward (new-reward uint))
  (begin
    (var-set base-review-reward new-reward)
    (ok true)
  )
)

(define-public (set-fast-track-cost (new-cost uint))
  (begin
    (var-set fast-track-cost new-cost)
    (ok true)
  )
)
