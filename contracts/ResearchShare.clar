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
(define-map paper-content-hashes
    { paper-id: uint }
    { 
        title-hash: (string-ascii 64),
        abstract-hash: (string-ascii 64),
        full-content-hash: (string-ascii 64)
    }
)

(define-map plagiarism-reports
    { paper-id: uint, compared-paper-id: uint }
    {
        similarity-score: uint,
        report-timestamp: uint,
        detected-by: principal,
        status: (string-ascii 32)
    }
)

(define-map paper-plagiarism-status
    { paper-id: uint }
    {
        is-flagged: bool,
        highest-similarity: uint,
        total-reports: uint,
        last-check: uint
    }
)

(define-data-var plagiarism-threshold uint u75)

(define-constant ERR_INVALID_SIMILARITY u7)
(define-constant ERR_DUPLICATE_REPORT u8)
(define-constant ERR_INVALID_THRESHOLD u9)
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

    (asserts! (is-some (map-get? papers { paper-id: paper-id })) (err u404))
    
    ;; Check if reviewer has already reviewed this paper

    (asserts! (is-none (map-get? reviews { paper-id: paper-id, reviewer: reviewer })) (err u403))
    (unwrap! (contract-call? .ReviewIncentives set-review-reward paper-id tx-sender) (err u500))

    ;; Check if score is valid (between 1 and 10)

    (asserts! (and (>= score u1) (<= score u10)) (err u400))
    
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

(define-read-only (get-review-score (paper-id uint) (reviewer principal))
  (match (map-get? reviews { paper-id: paper-id, reviewer: reviewer })
    review (ok (get score review))
    (err ERR_NOT_FOUND)
  )
)
(define-read-only (get-review-comment (paper-id uint) (reviewer principal))
  (match (map-get? reviews { paper-id: paper-id, reviewer: reviewer })
    review (ok (get comment review))
    (err ERR_NOT_FOUND)
  )
)


(define-public (store-content-hashes (paper-id uint) (title-hash (string-ascii 64)) (abstract-hash (string-ascii 64)) (full-content-hash (string-ascii 64)))
    (let
        (
            (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
        )
        (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
        (ok (map-set paper-content-hashes
            { paper-id: paper-id }
            {
                title-hash: title-hash,
                abstract-hash: abstract-hash,
                full-content-hash: full-content-hash
            }
        ))
    )
)

(define-public (report-plagiarism (paper-id uint) (compared-paper-id uint) (similarity-score uint))
    (let
        (
            (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
            (compared-paper (unwrap! (map-get? papers { paper-id: compared-paper-id }) (err ERR_NOT_FOUND)))
            (existing-report (map-get? plagiarism-reports { paper-id: paper-id, compared-paper-id: compared-paper-id }))
            (current-status (default-to { is-flagged: false, highest-similarity: u0, total-reports: u0, last-check: u0 } 
                (map-get? paper-plagiarism-status { paper-id: paper-id })))
        )
        (asserts! (not (is-eq paper-id compared-paper-id)) (err ERR_SELF_CITATION))
        (asserts! (<= similarity-score u100) (err ERR_INVALID_SIMILARITY))
        (asserts! (is-none existing-report) (err ERR_DUPLICATE_REPORT))
        
        (map-set plagiarism-reports
            { paper-id: paper-id, compared-paper-id: compared-paper-id }
            {
                similarity-score: similarity-score,
                report-timestamp: stacks-block-height,
                detected-by: tx-sender,
                status: (if (>= similarity-score (var-get plagiarism-threshold)) "flagged" "clean")
            }
        )
        
        (map-set paper-plagiarism-status
            { paper-id: paper-id }
            {
                is-flagged: (or (get is-flagged current-status) (>= similarity-score (var-get plagiarism-threshold))),
                highest-similarity: (if (> similarity-score (get highest-similarity current-status)) similarity-score (get highest-similarity current-status)),
                total-reports: (+ (get total-reports current-status) u1),
                last-check: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (update-plagiarism-threshold (new-threshold uint))
    (begin
        (asserts! (and (>= new-threshold u1) (<= new-threshold u100)) (err ERR_INVALID_THRESHOLD))
        (var-set plagiarism-threshold new-threshold)
        (ok true)
    )
)

(define-read-only (get-plagiarism-status (paper-id uint))
    (match (map-get? paper-plagiarism-status { paper-id: paper-id })
        status (ok status)
        (err ERR_NOT_FOUND)
    )
)

(define-read-only (get-plagiarism-report (paper-id uint) (compared-paper-id uint))
    (match (map-get? plagiarism-reports { paper-id: paper-id, compared-paper-id: compared-paper-id })
        report (ok report)
        (err ERR_NOT_FOUND)
    )
)

(define-read-only (get-content-hashes (paper-id uint))
    (match (map-get? paper-content-hashes { paper-id: paper-id })
        hashes (ok hashes)
        (err ERR_NOT_FOUND)
    )
)

(define-read-only (get-current-plagiarism-threshold)
    (var-get plagiarism-threshold)
)

(define-read-only (is-paper-flagged (paper-id uint))
    (match (map-get? paper-plagiarism-status { paper-id: paper-id })
        status (ok (get is-flagged status))
        (ok false)
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


;; Add at the top with other data structures
(define-map paper-categories 
    { paper-id: uint }
    { categories: (list 10 (string-ascii 64)) }
)

;; Add this public function
(define-public (add-categories (paper-id uint) (categories (list 10 (string-ascii 64))))
    (let (
        (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
    )
    (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
    (ok (map-set paper-categories { paper-id: paper-id } { categories: categories }))
    )
)


;; Add with other data structures
(define-map paper-stats
    { paper-id: uint }
    { view-count: uint }
)

;; Add this public function
(define-public (increment-views (paper-id uint))
    (let (
        (current-views (default-to u0 (get view-count (map-get? paper-stats { paper-id: paper-id }))))
    )
    (ok (map-set paper-stats 
        { paper-id: paper-id }
        { view-count: (+ current-views u1) }))
    )
)


;; Add with other data structures
(define-map author-reputation
    { author: principal }
    { 
        total-papers: uint,
        total-citations: uint,
        reputation-score: uint
    }
)

;; Add this function
(define-public (update-author-reputation (author principal))
    (let (
        (current-rep (default-to { total-papers: u0, total-citations: u0, reputation-score: u0 }
            (map-get? author-reputation { author: author })))
    )
    (ok (map-set author-reputation
        { author: author }
        (merge current-rep { reputation-score: (+ (get total-papers current-rep) 
            (* (get total-citations current-rep) u2)) })))
    )
)



;; Add with other data structures
(define-map paper-keywords
    { paper-id: uint }
    { keywords: (list 20 (string-ascii 64)) }
)

(define-public (set-keywords (paper-id uint) (keywords (list 20 (string-ascii 64))))
    (let (
        (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
    )
    (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
    (ok (map-set paper-keywords
        { paper-id: paper-id }
        { keywords: keywords }))
    )
)


;; Add with other data structures
(define-map review-votes
    { paper-id: uint, reviewer: principal, voter: principal }
    { vote: bool }
)

(define-public (vote-on-review (paper-id uint) (reviewer principal) (upvote bool))
    (let (
        (review (unwrap! (map-get? reviews { paper-id: paper-id, reviewer: reviewer }) (err ERR_NOT_FOUND)))
    )
    (ok (map-set review-votes
        { paper-id: paper-id, reviewer: reviewer, voter: tx-sender }
        { vote: upvote }))
    )
)


;; Add with other data structures
(define-map paper-funding
    { paper-id: uint }
    { 
        total-funds: uint,
        funders: (list 100 principal)
    }
)

(define-public (fund-paper (paper-id uint) (amount uint))
    (let (
        (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
        (current-funding (default-to { total-funds: u0, funders: (list) } 
            (map-get? paper-funding { paper-id: paper-id })))
        (current-funders (get funders current-funding))
    )
    (asserts! (< (len current-funders) u100) (err u100))
    (map-set paper-funding
        { paper-id: paper-id }
        { 
            total-funds: (+ (get total-funds current-funding) amount),
            funders: (unwrap! (as-max-len? (append current-funders tx-sender) u100) (err u101))
        })
    (ok true))
)


;; Add with other data structures
(define-map paper-access
    { paper-id: uint }
    {
        is-private: bool,
        allowed-readers: (list 50 principal)
    }
)

(define-public (set-paper-privacy (paper-id uint) (is-private bool) (allowed-readers (list 50 principal)))
    (let (
        (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
    )
    (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
    (ok (map-set paper-access
        { paper-id: paper-id }
        { 
            is-private: is-private,
            allowed-readers: allowed-readers
        }))
    )
)


;; Add with other data structures
(define-map verified-institutions 
    { institution: principal }
    { 
        name: (string-ascii 256),
        verified: bool
    }
)

(define-map author-institutions
    { author: principal }
    { institution: principal }
)

(define-public (link-institution (institution principal))
    (ok (map-set author-institutions
        { author: tx-sender }
        { institution: institution }))
)


;; Add with other data structures
(define-map paper-discussions
    { paper-id: uint, comment-id: uint }
    {
        author: principal,
        content: (string-ascii 512),
        timestamp: uint,
        parent-id: (optional uint)
    }
)

(define-data-var comment-count uint u0)

(define-public (add-comment (paper-id uint) (content (string-ascii 512)) (parent-id (optional uint)))
    (let (
        (new-id (+ (var-get comment-count) u1))
    )
    (var-set comment-count new-id)
    (ok (map-set paper-discussions
        { paper-id: paper-id, comment-id: new-id }
        {
            author: tx-sender,
            content: content,
            timestamp: stacks-block-height,
            parent-id: parent-id
        })))
)


;; Add with other data structures
(define-map research-fields
    { field-id: uint }
    { 
        name: (string-ascii 64),
        parent-field: (optional uint)
    }
)

(define-map paper-fields
    { paper-id: uint }
    { fields: (list 5 uint) }
)

(define-public (classify-paper (paper-id uint) (field-ids (list 5 uint)))
    (let (
        (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
    )
    (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
    (ok (map-set paper-fields
        { paper-id: paper-id }
        { fields: field-ids }))
))



(define-map paper-versions
    { paper-id: uint, version: uint }
    {
        content-hash: (string-ascii 64),
        changes: (string-ascii 512),
        timestamp: uint
    }
)

(define-map paper-version-count
    { paper-id: uint }
    { current-version: uint }
)

(define-public (publish-new-version (paper-id uint) (content-hash (string-ascii 64)) (changes (string-ascii 512)))
    (let
        (
            (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
            (current-version (default-to { current-version: u0 } (map-get? paper-version-count { paper-id: paper-id })))
            (new-version-num (+ (get current-version current-version) u1))
        )
        (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
        (map-set paper-versions
            { paper-id: paper-id, version: new-version-num }
            {
                content-hash: content-hash,
                changes: changes,
                timestamp: stacks-block-height
            }
        )
        (map-set paper-version-count
            { paper-id: paper-id }
            { current-version: new-version-num }
        )
        (ok new-version-num)
    )
)


(define-map paper-collaborators
    { paper-id: uint, author: principal }
    {
        role: (string-ascii 32),
        contribution: (string-ascii 256),
        approved: bool
    }
)

(define-public (add-collaborator (paper-id uint) (collaborator principal) (role (string-ascii 32)) (contribution (string-ascii 256)))
    (let
        (
            (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
        )
        (asserts! (is-eq tx-sender (get author paper)) (err ERR_UNAUTHORIZED))
        (ok (map-set paper-collaborators
            { paper-id: paper-id, author: collaborator }
            {
                role: role,
                contribution: contribution,
                approved: false
            }
        ))
    )
)

(define-public (approve-collaboration (paper-id uint))
    (let
        (
            (collaboration (unwrap! (map-get? paper-collaborators { paper-id: paper-id, author: tx-sender }) (err ERR_NOT_FOUND)))
        )
        (ok (map-set paper-collaborators
            { paper-id: paper-id, author: tx-sender }
            (merge collaboration { approved: true })
        ))
    )
)


(define-public (get-collaborators (paper-id uint))
    (let
        (
            (collaborators (unwrap! (map-get? paper-collaborators { paper-id: paper-id, author: tx-sender }) (err ERR_NOT_FOUND)))
        )
        (ok collaborators)
    )
)
(define-public (get-collaborator (paper-id uint) (collaborator principal))
    (let
        (
            (collaboration (unwrap! (map-get? paper-collaborators { paper-id: paper-id, author: collaborator }) (err ERR_NOT_FOUND)))
        )
        (ok collaboration)
    )
)
(define-public (get-paper-collaborators (paper-id uint))
    (let
        (
            (collaborations (unwrap! (map-get? paper-collaborators { paper-id: paper-id, author: tx-sender }) (err ERR_NOT_FOUND)))
        )
        (ok collaborations)
    )
)
(define-public (get-paper-collaborator (paper-id uint) (collaborator principal))
    (let
        (
            (collaboration (unwrap! (map-get? paper-collaborators { paper-id: paper-id, author: collaborator }) (err ERR_NOT_FOUND)))
        )
        (ok collaboration)
    )
)