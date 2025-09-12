;; Research Impact Analytics System
;; Advanced metrics and reputation tracking for academic research
;; Features: H-index calculation, cross-disciplinary impact, temporal analysis, collaborative metrics

;; Data structures for impact tracking
(define-map author-metrics
    { author: principal }
    {
        h-index: uint,
        total-citations: uint,
        papers-count: uint,
        collaboration-score: uint,
        interdisciplinary-score: uint,
        recent-impact-score: uint,
        last-updated: uint
    }
)

(define-map paper-impact-metrics
    { paper-id: uint }
    {
        citation-velocity: uint,        ;; Citations per time period
        cross-field-citations: uint,    ;; Citations from different fields
        collaboration-impact: uint,     ;; Impact from collaborative work
        recency-weighted-citations: uint,
        field-normalized-score: uint,
        temporal-peak: uint             ;; Block height of peak citation activity
    }
)

(define-map field-citation-network
    { source-field: (string-ascii 64), target-field: (string-ascii 64) }
    {
        citation-count: uint,
        last-citation: uint
    }
)

(define-map temporal-citation-patterns
    { paper-id: uint, time-window: uint }
    {
        citations-in-window: uint,
        velocity: uint,
        acceleration: uint
    }
)

(define-map author-collaboration-network
    { author1: principal, author2: principal }
    {
        shared-papers: uint,
        combined-citations: uint,
        collaboration-strength: uint
    }
)

;; Constants and variables
(define-data-var time-window-blocks uint u1440)  ;; ~10 days for velocity calculations
(define-data-var recency-decay-factor uint u95)  ;; 95% decay factor for temporal weighting
(define-data-var cross-field-bonus uint u150)    ;; 1.5x multiplier for cross-field citations

;; Error constants
(define-constant ERR_INVALID_PAPER u20)
(define-constant ERR_INVALID_AUTHOR u21)
(define-constant ERR_CALCULATION_ERROR u22)

;; Update impact metrics when a new citation is added
(define-public (record-citation-impact (paper-id uint) (citing-paper-id uint) (source-field (string-ascii 64)) (target-field (string-ascii 64)))
    (let
        (
            (current-block stacks-block-height)
        )
        ;; Update paper-level metrics
        (try! (update-paper-citation-velocity paper-id))
        (try! (update-cross-field-impact paper-id source-field target-field))
        (try! (update-temporal-citation-pattern paper-id current-block))
        
        ;; Update field network
        (try! (update-field-citation-network source-field target-field))
        
        (ok true)
    )
)

;; Calculate and update author H-index
(define-public (calculate-h-index (author principal) (citation-counts (list 100 uint)))
    (let
        (
            (sorted-citations (sort-citations citation-counts))
            (h-value (calculate-h-value sorted-citations u0))
            (current-metrics (default-to 
                { h-index: u0, total-citations: u0, papers-count: u0, collaboration-score: u0, 
                  interdisciplinary-score: u0, recent-impact-score: u0, last-updated: u0 }
                (map-get? author-metrics { author: author })))
        )
        (map-set author-metrics
            { author: author }
            (merge current-metrics { 
                h-index: h-value,
                last-updated: stacks-block-height 
            })
        )
        (ok h-value)
    )
)

;; Update author collaboration metrics
(define-public (update-collaboration-metrics (author1 principal) (author2 principal) (paper-citations uint))
    (let
        (
            (current-collab (default-to { shared-papers: u0, combined-citations: u0, collaboration-strength: u0 }
                (map-get? author-collaboration-network { author1: author1, author2: author2 })))
            (new-shared-papers (+ (get shared-papers current-collab) u1))
            (new-combined-citations (+ (get combined-citations current-collab) paper-citations))
            (collaboration-strength (/ (* new-shared-papers new-combined-citations) u10))
        )
        (map-set author-collaboration-network
            { author1: author1, author2: author2 }
            {
                shared-papers: new-shared-papers,
                combined-citations: new-combined-citations,
                collaboration-strength: collaboration-strength
            }
        )
        
        ;; Update both authors' collaboration scores
        (try! (update-author-collaboration-score author1))
        (try! (update-author-collaboration-score author2))
        (ok true)
    )
)

;; Private helper functions

(define-private (update-paper-citation-velocity (paper-id uint))
    (let
        (
            (current-metrics (default-to 
                { citation-velocity: u0, cross-field-citations: u0, collaboration-impact: u0, 
                  recency-weighted-citations: u0, field-normalized-score: u0, temporal-peak: u0 }
                (map-get? paper-impact-metrics { paper-id: paper-id })))
            (time-window (var-get time-window-blocks))
        )
        (map-set paper-impact-metrics
            { paper-id: paper-id }
            (merge current-metrics { 
                citation-velocity: (+ (get citation-velocity current-metrics) u1)
            })
        )
        (ok true)
    )
)

(define-private (update-cross-field-impact (paper-id uint) (source-field (string-ascii 64)) (target-field (string-ascii 64)))
    (let
        (
            (is-cross-field (not (is-eq source-field target-field)))
            (current-metrics (unwrap-panic (map-get? paper-impact-metrics { paper-id: paper-id })))
        )
        (if is-cross-field
            (map-set paper-impact-metrics
                { paper-id: paper-id }
                (merge current-metrics { 
                    cross-field-citations: (+ (get cross-field-citations current-metrics) u1)
                })
            )
            true
        )
        (ok true)
    )
)

(define-private (update-temporal-citation-pattern (paper-id uint) (citation-block uint))
    (let
        (
            (time-window u1000)  ;; Approximately 7 days
            (window-id (/ citation-block time-window))
            (current-pattern (default-to { citations-in-window: u0, velocity: u0, acceleration: u0 }
                (map-get? temporal-citation-patterns { paper-id: paper-id, time-window: window-id })))
        )
        (map-set temporal-citation-patterns
            { paper-id: paper-id, time-window: window-id }
            (merge current-pattern { 
                citations-in-window: (+ (get citations-in-window current-pattern) u1)
            })
        )
        (ok true)
    )
)

(define-private (update-field-citation-network (source-field (string-ascii 64)) (target-field (string-ascii 64)))
    (let
        (
            (current-network (default-to { citation-count: u0, last-citation: u0 }
                (map-get? field-citation-network { source-field: source-field, target-field: target-field })))
        )
        (map-set field-citation-network
            { source-field: source-field, target-field: target-field }
            {
                citation-count: (+ (get citation-count current-network) u1),
                last-citation: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-private (update-author-collaboration-score (author principal))
    (let
        (
            (current-metrics (unwrap-panic (map-get? author-metrics { author: author })))
        )
        ;; Simplified collaboration score calculation
        (map-set author-metrics
            { author: author }
            (merge current-metrics { 
                collaboration-score: (+ (get collaboration-score current-metrics) u10)
            })
        )
        (ok true)
    )
)

(define-private (sort-citations (citations (list 100 uint)))
    ;; Simplified sort - in real implementation would need proper sorting
    citations
)

(define-private (calculate-h-value (sorted-citations (list 100 uint)) (index uint))
    ;; Simplified H-index calculation
    ;; In real implementation, would iterate through sorted list
    (fold calculate-h-helper sorted-citations u0)
)

(define-private (calculate-h-helper (citations uint) (h-so-far uint))
    (if (>= citations h-so-far)
        (+ h-so-far u1)
        h-so-far
    )
)

;; Read-only functions for analytics

(define-read-only (get-author-metrics (author principal))
    (map-get? author-metrics { author: author })
)

(define-read-only (get-paper-impact-metrics (paper-id uint))
    (map-get? paper-impact-metrics { paper-id: paper-id })
)

(define-read-only (get-field-citation-network (source-field (string-ascii 64)) (target-field (string-ascii 64)))
    (map-get? field-citation-network { source-field: source-field, target-field: target-field })
)

(define-read-only (get-collaboration-strength (author1 principal) (author2 principal))
    (match (map-get? author-collaboration-network { author1: author1, author2: author2 })
        collab (ok (get collaboration-strength collab))
        (match (map-get? author-collaboration-network { author1: author2, author2: author1 })
            collab (ok (get collaboration-strength collab))
            (ok u0)
        )
    )
)

(define-read-only (calculate-interdisciplinary-impact (paper-id uint))
    (match (map-get? paper-impact-metrics { paper-id: paper-id })
        metrics (ok (* (get cross-field-citations metrics) (var-get cross-field-bonus)))
        (ok u0)
    )
)

(define-read-only (get-citation-velocity (paper-id uint))
    (match (map-get? paper-impact-metrics { paper-id: paper-id })
        metrics (ok (get citation-velocity metrics))
        (ok u0)
    )
)
