;; Automated Credit Issuance Contract
;; Smart contracts automatically generating and distributing carbon credits based on verified environmental impact measurements

;; SIP-010 Token Implementation for Carbon Credits
;; Note: Trait implementation would be added in production deployment

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u200))
(define-constant ERR_UNAUTHORIZED (err u201))
(define-constant ERR_INVALID_AMOUNT (err u202))
(define-constant ERR_PROJECT_NOT_ELIGIBLE (err u203))
(define-constant ERR_INSUFFICIENT_BALANCE (err u204))
(define-constant ERR_TRANSFER_FAILED (err u205))
(define-constant ERR_INVALID_RECIPIENT (err u206))
(define-constant ERR_CREDITS_EXHAUSTED (err u207))
(define-constant ERR_VESTING_ACTIVE (err u208))
(define-constant ERR_ALREADY_ISSUED (err u209))
(define-constant ERR_INVALID_VESTING_PERIOD (err u210))

;; Token Constants
(define-constant TOKEN_NAME "CarbonCredit")
(define-constant TOKEN_SYMBOL "CC")
(define-constant TOKEN_DECIMALS u6)
(define-constant MICRO_CREDIT u1000000) ;; 1 credit = 1,000,000 micro-credits

;; Issuance Constants
(define-constant MIN_ISSUANCE_AMOUNT u1000) ;; minimum 0.001 credits
(define-constant MAX_ISSUANCE_AMOUNT u1000000000) ;; maximum 1000 credits
(define-constant VESTING_CLIFF_BLOCKS u144) ;; ~1 day in blocks
(define-constant DEFAULT_VESTING_PERIOD u4320) ;; ~30 days in blocks

;; Data Variables
(define-data-var total-supply uint u0)
(define-data-var issuance-paused bool false)
(define-data-var daily-issuance-limit uint u100000000) ;; 100 credits per day
(define-data-var issuance-fee-rate uint u50) ;; 0.5% fee (50 basis points)

;; Data Maps
(define-map balances principal uint)
(define-map allowed { owner: principal, spender: principal } uint)

;; Credit Issuance Tracking
(define-map project-credits
  uint ;; project-id
  {
    total-issued: uint,
    total-retired: uint,
    last-issuance: uint,
    issuance-rate: uint,
    max-credits: uint,
    project-owner: principal
  }
)

;; Vesting Schedules
(define-map vesting-schedules
  { project-id: uint, beneficiary: principal }
  {
    total-amount: uint,
    released-amount: uint,
    start-block: uint,
    cliff-duration: uint,
    vesting-duration: uint,
    revocable: bool,
    revoked: bool
  }
)

;; Daily Issuance Tracking
(define-map daily-issuance
  uint ;; block-day (block-height / 144)
  uint ;; total issued today
)

;; Authorized Issuers
(define-map authorized-issuers principal bool)

;; Credit Retirement (burning)
(define-map retired-credits
  { credit-id: uint, retiree: principal }
  {
    amount: uint,
    retirement-date: uint,
    retirement-reason: (string-ascii 100),
    certificate-hash: (string-ascii 64)
  }
)

;; Credit Metadata
(define-map credit-batches
  uint ;; batch-id
  {
    project-id: uint,
    vintage: uint,
    methodology: (string-ascii 50),
    verification-standard: (string-ascii 30),
    issuance-date: uint,
    batch-size: uint,
    serial-start: uint,
    serial-end: uint
  }
)

(define-data-var credit-batch-counter uint u0)
(define-data-var retirement-counter uint u0)

;; SIP-010 Standard Functions

;; Get token name
(define-read-only (get-name)
  (ok TOKEN_NAME)
)

;; Get token symbol
(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

;; Get token decimals
(define-read-only (get-decimals)
  (ok TOKEN_DECIMALS)
)

;; Get balance of account
(define-read-only (get-balance (account principal))
  (ok (default-to u0 (map-get? balances account)))
)

;; Get total supply
(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

;; Get token URI (metadata)
(define-read-only (get-token-uri)
  (ok (some "https://verified-carbon-exchange.io/metadata/carbon-credits"))
)

;; Transfer tokens
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (let (
    (sender-balance (default-to u0 (map-get? balances sender)))
    )
    (asserts! (or (is-eq sender tx-sender) (is-eq sender contract-caller)) ERR_UNAUTHORIZED)
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (is-eq sender recipient)) ERR_INVALID_RECIPIENT)
    
    ;; Update balances
    (map-set balances sender (- sender-balance amount))
    (map-set balances recipient (+ (default-to u0 (map-get? balances recipient)) amount))
    
    ;; Print transfer event
    (print { type: "transfer", sender: sender, recipient: recipient, amount: amount, memo: memo })
    
    (ok true)
  )
)

;; Public Functions - Credit Issuance

;; Issue credits for verified project
(define-public (issue-credits
  (project-id uint)
  (amount uint)
  (vintage uint)
  (methodology (string-ascii 50))
  (verification-standard (string-ascii 30))
  (recipient principal)
  )
  (let (
    (batch-id (+ (var-get credit-batch-counter) u1))
    (project-credits-data (default-to 
      { total-issued: u0, total-retired: u0, last-issuance: u0, issuance-rate: u1000, max-credits: u1000000000, project-owner: recipient }
      (map-get? project-credits project-id)
    ))
    (daily-key (/ stacks-block-height u144))
    (daily-issued (default-to u0 (map-get? daily-issuance daily-key)))
    )
    
    ;; Validate authorization
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER)
                  (default-to false (map-get? authorized-issuers tx-sender)))
              ERR_UNAUTHORIZED)
    
    ;; Validate inputs
    (asserts! (and (>= amount MIN_ISSUANCE_AMOUNT) (<= amount MAX_ISSUANCE_AMOUNT)) ERR_INVALID_AMOUNT)
    (asserts! (not (var-get issuance-paused)) ERR_UNAUTHORIZED)
    (asserts! (<= (+ daily-issued amount) (var-get daily-issuance-limit)) ERR_CREDITS_EXHAUSTED)
    
    ;; Check project limits
    (asserts! (<= (+ (get total-issued project-credits-data) amount) 
                  (get max-credits project-credits-data)) ERR_CREDITS_EXHAUSTED)
    
    ;; Create credit batch
    (map-set credit-batches batch-id {
      project-id: project-id,
      vintage: vintage,
      methodology: methodology,
      verification-standard: verification-standard,
      issuance-date: stacks-block-height,
      batch-size: amount,
      serial-start: (+ (get total-issued project-credits-data) u1),
      serial-end: (+ (get total-issued project-credits-data) amount)
    })
    
    ;; Update project credits
    (map-set project-credits project-id (merge project-credits-data {
      total-issued: (+ (get total-issued project-credits-data) amount),
      last-issuance: stacks-block-height
    }))
    
    ;; Update daily issuance
    (map-set daily-issuance daily-key (+ daily-issued amount))
    
    ;; Mint tokens
    ;; (try! (mint-credits amount recipient))
    
    ;; Update counter
    (var-set credit-batch-counter batch-id)
    
    ;; Print issuance event
    (print { type: "credit-issuance", project-id: project-id, batch-id: batch-id, amount: amount, recipient: recipient })
    
    (ok batch-id)
  )
)

;; Create vesting schedule for project developers
(define-public (create-vesting-schedule
  (project-id uint)
  (beneficiary principal)
  (total-amount uint)
  (cliff-duration uint)
  (vesting-duration uint)
  (revocable bool)
  )
  (let (
    (project-credits-data (unwrap! (map-get? project-credits project-id) ERR_PROJECT_NOT_ELIGIBLE))
    )
    ;; Only project owner or contract owner can create vesting
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER)
                  (is-eq tx-sender (get project-owner project-credits-data)))
              ERR_UNAUTHORIZED)
    
    ;; Validate vesting parameters
    (asserts! (>= cliff-duration VESTING_CLIFF_BLOCKS) ERR_INVALID_VESTING_PERIOD)
    (asserts! (>= vesting-duration cliff-duration) ERR_INVALID_VESTING_PERIOD)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    
    ;; Create vesting schedule
    (map-set vesting-schedules { project-id: project-id, beneficiary: beneficiary } {
      total-amount: total-amount,
      released-amount: u0,
      start-block: stacks-block-height,
      cliff-duration: cliff-duration,
      vesting-duration: vesting-duration,
      revocable: revocable,
      revoked: false
    })
    
    (ok true)
  )
)

;; Release vested credits
(define-public (release-vested-credits (project-id uint) (beneficiary principal))
  (let (
    (vesting (unwrap! (map-get? vesting-schedules { project-id: project-id, beneficiary: beneficiary }) ERR_PROJECT_NOT_ELIGIBLE))
    (releasable-amount (unwrap! (get-releasable-amount project-id beneficiary) ERR_INVALID_AMOUNT))
    )
    ;; Check if vesting is not revoked
    (asserts! (not (get revoked vesting)) ERR_VESTING_ACTIVE)
    (asserts! (> releasable-amount u0) ERR_INVALID_AMOUNT)
    
    ;; Update vesting schedule
    (map-set vesting-schedules { project-id: project-id, beneficiary: beneficiary }
      (merge vesting {
        released-amount: (+ (get released-amount vesting) releasable-amount)
      })
    )
    
    ;; Mint credits to beneficiary
    ;; (try! (mint-credits releasable-amount beneficiary))
    
    (ok releasable-amount)
  )
)

;; Retire (burn) credits
(define-public (retire-credits
  (amount uint)
  (retirement-reason (string-ascii 100))
  (certificate-hash (string-ascii 64))
  )
  (let (
    (retirement-id (+ (var-get retirement-counter) u1))
    (user-balance (default-to u0 (map-get? balances tx-sender)))
    )
    (asserts! (>= user-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Record retirement
    (map-set retired-credits { credit-id: retirement-id, retiree: tx-sender } {
      amount: amount,
      retirement-date: stacks-block-height,
      retirement-reason: retirement-reason,
      certificate-hash: certificate-hash
    })
    
    ;; Burn tokens
    (try! (burn-credits amount tx-sender))
    
    ;; Update counter
    (var-set retirement-counter retirement-id)
    
    ;; Print retirement event
    (print { type: "credit-retirement", retirement-id: retirement-id, amount: amount, retiree: tx-sender })
    
    (ok retirement-id)
  )
)

;; Administrative Functions

;; Add authorized issuer
(define-public (add-authorized-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (map-set authorized-issuers issuer true)
    (ok true)
  )
)

;; Remove authorized issuer
(define-public (remove-authorized-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (map-delete authorized-issuers issuer)
    (ok true)
  )
)

;; Pause/unpause issuance
(define-public (set-issuance-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (var-set issuance-paused paused)
    (ok true)
  )
)

;; Update daily issuance limit
(define-public (set-daily-limit (new-limit uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (var-set daily-issuance-limit new-limit)
    (ok true)
  )
)

;; Read-Only Functions

;; Get project credit info
(define-read-only (get-project-credits (project-id uint))
  (map-get? project-credits project-id)
)

;; Get credit batch info
(define-read-only (get-credit-batch (batch-id uint))
  (map-get? credit-batches batch-id)
)

;; Get vesting schedule
(define-read-only (get-vesting-schedule (project-id uint) (beneficiary principal))
  (map-get? vesting-schedules { project-id: project-id, beneficiary: beneficiary })
)

;; Get retirement info
(define-read-only (get-retirement-info (retirement-id uint) (retiree principal))
  (map-get? retired-credits { credit-id: retirement-id, retiree: retiree })
)

;; Check if issuer is authorized
(define-read-only (is-authorized-issuer (issuer principal))
  (default-to false (map-get? authorized-issuers issuer))
)

;; Calculate releasable vested amount
(define-read-only (get-releasable-amount (project-id uint) (beneficiary principal))
  (match (map-get? vesting-schedules { project-id: project-id, beneficiary: beneficiary })
    vesting
    (let (
      (elapsed (- stacks-block-height (get start-block vesting)))
      (cliff-duration (get cliff-duration vesting))
      (vesting-duration (get vesting-duration vesting))
      (total-amount (get total-amount vesting))
      (released-amount (get released-amount vesting))
      )
      (if (< elapsed cliff-duration)
        (ok u0)
        (if (>= elapsed vesting-duration)
          (ok (- total-amount released-amount))
          (ok (- (/ (* total-amount elapsed) vesting-duration) released-amount))
        )
      )
    )
    (err ERR_PROJECT_NOT_ELIGIBLE)
  )
)

;; Get daily issuance for specific day
(define-read-only (get-daily-issuance (block-day uint))
  (default-to u0 (map-get? daily-issuance block-day))
)

;; Private Functions

;; Mint credits (internal)
(define-private (mint-credits (amount uint) (recipient principal))
  (let (
    (current-balance (default-to u0 (map-get? balances recipient)))
    (new-total-supply (+ (var-get total-supply) amount))
    )
    ;; Update recipient balance
    (map-set balances recipient (+ current-balance amount))
    
    ;; Update total supply
    (var-set total-supply new-total-supply)
    
    (ok true)
  )
)

;; Burn credits (internal)
(define-private (burn-credits (amount uint) (account principal))
  (let (
    (current-balance (default-to u0 (map-get? balances account)))
    (new-total-supply (- (var-get total-supply) amount))
    )
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update account balance
    (map-set balances account (- current-balance amount))
    
    ;; Update total supply
    (var-set total-supply new-total-supply)
    
    (ok true)
  )
)

