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

;; Research Bounty Marketplace System
;; Enables researchers to post problems with STX rewards for solutions

;; Bounty data structures
(define-map research-bounties
    { bounty-id: uint }
    {
        creator: principal,
        title: (string-ascii 256),
        description: (string-ascii 1024),
        reward-amount: uint,
        deadline: uint,
        status: (string-ascii 32),
        field: (string-ascii 64),
        difficulty: uint,
        created-at: uint
    }
)

(define-map bounty-submissions
    { bounty-id: uint, submitter: principal }
    {
        paper-id: uint,
        solution-description: (string-ascii 512),
        submitted-at: uint,
        score: uint,
        evaluated: bool
    }
)

(define-map bounty-evaluations
    { bounty-id: uint, submission-id: uint }
    {
        evaluator: principal,
        score: uint,
        feedback: (string-ascii 256),
        evaluated-at: uint
    }
)

(define-map bounty-awards
    { bounty-id: uint }
    {
        winner: principal,
        awarded-at: uint,
        final-score: uint,
        total-submissions: uint
    }
)

(define-map bounty-disputes
    { bounty-id: uint, disputer: principal }
    {
        reason: (string-ascii 256),
        status: (string-ascii 32),
        created-at: uint,
        resolved-at: (optional uint)
    }
)

(define-map solver-reputation
    { solver: principal }
    {
        bounties-solved: uint,
        total-earnings: uint,
        average-score: uint,
        disputes-filed: uint
    }
)

(define-map creator-reputation
    { creator: principal }
    {
        bounties-created: uint,
        total-spent: uint,
        average-satisfaction: uint,
        disputes-against: uint
    }
)

;; Bounty system variables
(define-data-var bounty-count uint u0)
(define-data-var min-bounty-amount uint u1000000) ;; 1 STX minimum
(define-data-var max-bounty-duration uint u52560) ;; ~1 year in blocks
(define-data-var evaluation-period uint u2160) ;; ~15 days for evaluation

;; Bounty error codes
(define-constant ERR_INSUFFICIENT_BOUNTY u10)
(define-constant ERR_INVALID_DEADLINE u11)
(define-constant ERR_BOUNTY_EXPIRED u12)
(define-constant ERR_ALREADY_SUBMITTED u13)
(define-constant ERR_NOT_CREATOR u14)
(define-constant ERR_BOUNTY_COMPLETED u15)
(define-constant ERR_EVALUATION_PENDING u16)
(define-constant ERR_INVALID_BOUNTY_SCORE u17)
(define-constant ERR_DISPUTE_EXISTS u18)

;; Create a new research bounty
(define-public (create-bounty (title (string-ascii 256)) (description (string-ascii 1024)) (reward-amount uint) (deadline uint) (field (string-ascii 64)) (difficulty uint))
    (let
        (
            (new-bounty-id (+ (var-get bounty-count) u1))
            (current-block stacks-block-height)
        )
        ;; Validate bounty parameters
        (asserts! (>= reward-amount (var-get min-bounty-amount)) (err ERR_INSUFFICIENT_BOUNTY))
        (asserts! (> deadline current-block) (err ERR_INVALID_DEADLINE))
        (asserts! (<= (- deadline current-block) (var-get max-bounty-duration)) (err ERR_INVALID_DEADLINE))
        (asserts! (and (>= difficulty u1) (<= difficulty u10)) (err ERR_INVALID_BOUNTY_SCORE))
        
        ;; Transfer reward to contract (escrow)
        (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
        
        ;; Create bounty record
        (map-set research-bounties
            { bounty-id: new-bounty-id }
            {
                creator: tx-sender,
                title: title,
                description: description,
                reward-amount: reward-amount,
                deadline: deadline,
                status: "active",
                field: field,
                difficulty: difficulty,
                created-at: current-block
            }
        )
        
        ;; Update creator reputation
        (let
            (
                (current-rep (default-to { bounties-created: u0, total-spent: u0, average-satisfaction: u0, disputes-against: u0 }
                    (map-get? creator-reputation { creator: tx-sender })))
            )
            (map-set creator-reputation
                { creator: tx-sender }
                {
                    bounties-created: (+ (get bounties-created current-rep) u1),
                    total-spent: (+ (get total-spent current-rep) reward-amount),
                    average-satisfaction: (get average-satisfaction current-rep),
                    disputes-against: (get disputes-against current-rep)
                }
            )
        )
        
        (var-set bounty-count new-bounty-id)
        (ok new-bounty-id)
    )
)

;; Submit a solution to a bounty
(define-public (submit-solution (bounty-id uint) (paper-id uint) (solution-description (string-ascii 512)))
    (let
        (
            (bounty (unwrap! (map-get? research-bounties { bounty-id: bounty-id }) (err ERR_NOT_FOUND)))
            (paper (unwrap! (map-get? papers { paper-id: paper-id }) (err ERR_NOT_FOUND)))
            (existing-submission (map-get? bounty-submissions { bounty-id: bounty-id, submitter: tx-sender }))
        )
        ;; Validate submission
        (asserts! (is-eq (get status bounty) "active") (err ERR_BOUNTY_COMPLETED))
        (asserts! (< stacks-block-height (get deadline bounty)) (err ERR_BOUNTY_EXPIRED))
        (asserts! (is-none existing-submission) (err ERR_ALREADY_SUBMITTED))
        (asserts! (get verified paper) (err ERR_PAPER_NOT_VERIFIED))
        
        ;; Create submission record
        (map-set bounty-submissions
            { bounty-id: bounty-id, submitter: tx-sender }
            {
                paper-id: paper-id,
                solution-description: solution-description,
                submitted-at: stacks-block-height,
                score: u0,
                evaluated: false
            }
        )
        
        (ok true)
    )
)

;; Evaluate a bounty submission (creator only)
(define-public (evaluate-submission (bounty-id uint) (submitter principal) (score uint) (feedback (string-ascii 256)))
    (let
        (
            (bounty (unwrap! (map-get? research-bounties { bounty-id: bounty-id }) (err ERR_NOT_FOUND)))
            (submission (unwrap! (map-get? bounty-submissions { bounty-id: bounty-id, submitter: submitter }) (err ERR_NOT_FOUND)))
        )
        ;; Validate evaluation rights
        (asserts! (is-eq tx-sender (get creator bounty)) (err ERR_NOT_CREATOR))
        (asserts! (>= stacks-block-height (get deadline bounty)) (err ERR_EVALUATION_PENDING))
        (asserts! (and (>= score u1) (<= score u100)) (err ERR_INVALID_BOUNTY_SCORE))
        
        ;; Update submission with evaluation
        (map-set bounty-submissions
            { bounty-id: bounty-id, submitter: submitter }
            (merge submission { score: score, evaluated: true })
        )
        
        ;; Record evaluation details
        (map-set bounty-evaluations
            { bounty-id: bounty-id, submission-id: u1 }
            {
                evaluator: tx-sender,
                score: score,
                feedback: feedback,
                evaluated-at: stacks-block-height
            }
        )
        
        (ok true)
    )
)

;; Award bounty to winner (creator only)
(define-public (award-bounty (bounty-id uint) (winner principal))
    (let
        (
            (bounty (unwrap! (map-get? research-bounties { bounty-id: bounty-id }) (err ERR_NOT_FOUND)))
            (submission (unwrap! (map-get? bounty-submissions { bounty-id: bounty-id, submitter: winner }) (err ERR_NOT_FOUND)))
        )
        ;; Validate award conditions
        (asserts! (is-eq tx-sender (get creator bounty)) (err ERR_NOT_CREATOR))
        (asserts! (is-eq (get status bounty) "active") (err ERR_BOUNTY_COMPLETED))
        (asserts! (get evaluated submission) (err ERR_EVALUATION_PENDING))
        
        ;; Transfer reward to winner
        (try! (as-contract (stx-transfer? (get reward-amount bounty) tx-sender winner)))
        
        ;; Update bounty status
        (map-set research-bounties
            { bounty-id: bounty-id }
            (merge bounty { status: "completed" })
        )
        
        ;; Record award
        (map-set bounty-awards
            { bounty-id: bounty-id }
            {
                winner: winner,
                awarded-at: stacks-block-height,
                final-score: (get score submission),
                total-submissions: u1
            }
        )
        
        ;; Update solver reputation
        (let
            (
                (current-rep (default-to { bounties-solved: u0, total-earnings: u0, average-score: u0, disputes-filed: u0 }
                    (map-get? solver-reputation { solver: winner })))
            )
            (map-set solver-reputation
                { solver: winner }
                {
                    bounties-solved: (+ (get bounties-solved current-rep) u1),
                    total-earnings: (+ (get total-earnings current-rep) (get reward-amount bounty)),
                    average-score: (/ (+ (* (get average-score current-rep) (get bounties-solved current-rep)) (get score submission)) 
                                    (+ (get bounties-solved current-rep) u1)),
                    disputes-filed: (get disputes-filed current-rep)
                }
            )
        )
        
        (ok true)
    )
)

;; File dispute against bounty decision
(define-public (file-dispute (bounty-id uint) (reason (string-ascii 256)))
    (let
        (
            (bounty (unwrap! (map-get? research-bounties { bounty-id: bounty-id }) (err ERR_NOT_FOUND)))
            (existing-dispute (map-get? bounty-disputes { bounty-id: bounty-id, disputer: tx-sender }))
        )
        ;; Validate dispute conditions
        (asserts! (is-eq (get status bounty) "completed") (err ERR_BOUNTY_COMPLETED))
        (asserts! (is-none existing-dispute) (err ERR_DISPUTE_EXISTS))
        
        ;; Create dispute record
        (map-set bounty-disputes
            { bounty-id: bounty-id, disputer: tx-sender }
            {
                reason: reason,
                status: "pending",
                created-at: stacks-block-height,
                resolved-at: none
            }
        )
        
        (ok true)
    )
)

;; Read-only functions for bounty system

(define-read-only (get-bounty (bounty-id uint))
    (match (map-get? research-bounties { bounty-id: bounty-id })
        bounty (ok bounty)
        (err ERR_NOT_FOUND)
    )
)

(define-read-only (get-submission (bounty-id uint) (submitter principal))
    (match (map-get? bounty-submissions { bounty-id: bounty-id, submitter: submitter })
        submission (ok submission)
        (err ERR_NOT_FOUND)
    )
)

(define-read-only (get-solver-reputation (solver principal))
    (match (map-get? solver-reputation { solver: solver })
        reputation (ok reputation)
        (ok { bounties-solved: u0, total-earnings: u0, average-score: u0, disputes-filed: u0 })
    )
)

(define-read-only (get-creator-reputation (creator principal))
    (match (map-get? creator-reputation { creator: creator })
        reputation (ok reputation)
        (ok { bounties-created: u0, total-spent: u0, average-satisfaction: u0, disputes-against: u0 })
    )
)

(define-read-only (get-bounty-count)
    (var-get bounty-count)
)

(define-read-only (is-bounty-active (bounty-id uint))
    (match (map-get? research-bounties { bounty-id: bounty-id })
        bounty (ok (and (is-eq (get status bounty) "active") (< stacks-block-height (get deadline bounty))))
        (ok false)
    )
)
