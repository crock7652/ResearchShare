;; ResearchShare - Academic research sharing platform
;; Features: Paper verification, citation tracking, peer review

;; Define data structures
(define-map papers
  { paper-id: uint }
  {
    title: (string-ascii 256),
    author: principal,
    abstract: (string-ascii 1024),
    verified: bool,
    timestamp: uint,
    citation-count: uint
  }
)

(define-map reviews
  { paper-id: uint, reviewer: principal }
  {
    score: uint,
    comment: (string-ascii 512),
    timestamp: uint
  }
)

(define-map citations
  { paper-id: uint, citing-paper-id: uint }
  { timestamp: uint }
)

;; Define variables
(define-data-var paper-count uint u0)

;; Error codes
(define-constant ERR_NOT_FOUND u1)
(define-constant ERR_UNAUTHORIZED u2)
(define-constant ERR_ALREADY_REVIEWED u3)
(define-constant ERR_INVALID_SCORE u4)
(define-constant ERR_SELF_CITATION u5)
(define-constant ERR_PAPER_NOT_VERIFIED u6)

;; Functions

;; Submit a new research paper
(define-public (submit-paper (title (string-ascii 256)) (abstract (string-ascii 1024)))
  (let
    (
      (new-id (+ (var-get paper-count) u1))
    )
    (map-set papers
      { paper-id: new-id }
      {
        title: title,
        author: tx-sender,
        abstract: abstract,
        verified: false,
        timestamp: stacks-block-height,
        citation-count: u0
      }
    )
    (var-set paper-count new-id)
    (ok new-id)
  )
)

;; Get paper details
(define-read-only (get-paper (paper-id uint))
  (match (map-get? papers { paper-id: paper-id })
    paper (ok paper)
    (err ERR_NOT_FOUND)
  )
)

;; Verify a paper (could be restricted to certain principals in a real implementation)
(define-public (verify-paper (paper-id uint))
  (match (map-get? papers { paper-id: paper-id })
    paper 
      (begin
        (map-set papers
          { paper-id: paper-id }
          (merge paper { verified: true })
        )
        (ok true)
      )
    (err ERR_NOT_FOUND)
  )
)

;; Submit a review for a paper
(define-public (submit-review (paper-id uint) (score uint) (comment (string-ascii 512)))
  (let
    (
      (reviewer tx-sender)
    )
    ;; Check if paper exists
    (asserts! (is-some (map-get? papers { paper-id: paper-id })) (err ERR_NOT_FOUND))
    
    ;; Check if reviewer has already reviewed this paper
    (asserts! (is-none (map-get? reviews { paper-id: paper-id, reviewer: reviewer })) (err ERR_ALREADY_REVIEWED))
    
    ;; Check if score is valid (between 1 and 10)
    (asserts! (and (>= score u1) (<= score u10)) (err ERR_INVALID_SCORE))
    
    ;; Add review
    (map-set reviews
      { paper-id: paper-id, reviewer: reviewer }
      {
        score: score,
        comment: comment,
        timestamp: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Get a specific review
(define-read-only (get-review (paper-id uint) (reviewer principal))
  (match (map-get? reviews { paper-id: paper-id, reviewer: reviewer })
    review (ok review)
    (err ERR_NOT_FOUND)
  )
)

;; Add a citation
(define-public (add-citation (paper-id uint) (citing-paper-id uint))
  (let
    (
      (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
      (citing-paper (unwrap! (map-get? papers { paper-id: citing-paper-id }) (err ERR_NOT_FOUND)))
    )
    ;; Check if citing self
    (asserts! (not (is-eq paper-id citing-paper-id)) (err ERR_SELF_CITATION))
    
    ;; Check if citing paper is verified
    (asserts! (get verified citing-paper) (err ERR_PAPER_NOT_VERIFIED))
    
    ;; Add citation
    (map-set citations
      { paper-id: paper-id, citing-paper-id: citing-paper-id }
      { timestamp: stacks-block-height }
    )
    
    ;; Increment citation count
    (map-set papers
      { paper-id: paper-id }
      (merge paper { citation-count: (+ (get citation-count paper) u1) })
    )
    
    (ok true)
  )
)

;; Check if a paper cites another paper
(define-read-only (has-citation (paper-id uint) (citing-paper-id uint))
  (is-some (map-get? citations { paper-id: paper-id, citing-paper-id: citing-paper-id }))
)

;; Get total number of papers
(define-read-only (get-paper-count)
  (var-get paper-count)
)
