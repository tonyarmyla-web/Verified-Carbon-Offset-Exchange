;; Individual Carbon Wallet Contract
;; Personal carbon footprint calculator with gamified offset purchasing and community challenges for environmental action

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u400))
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_USER_NOT_FOUND (err u402))
(define-constant ERR_INVALID_FOOTPRINT (err u403))
(define-constant ERR_INSUFFICIENT_CREDITS (err u404))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u405))
(define-constant ERR_ALREADY_PARTICIPATED (err u406))
(define-constant ERR_CHALLENGE_ENDED (err u407))
(define-constant ERR_INVALID_ACHIEVEMENT (err u408))
(define-constant ERR_INVALID_ACTIVITY (err u409))
(define-constant ERR_GOAL_NOT_FOUND (err u410))

;; Activity Types for Carbon Footprint Calculation
(define-constant ACTIVITY_TRANSPORT u0)
(define-constant ACTIVITY_ENERGY u1)
(define-constant ACTIVITY_FOOD u2)
(define-constant ACTIVITY_SHOPPING u3)
(define-constant ACTIVITY_TRAVEL u4)
(define-constant ACTIVITY_WASTE u5)

;; Achievement Types
(define-constant ACHIEVEMENT_FIRST_OFFSET u0)
(define-constant ACHIEVEMENT_CARBON_NEUTRAL u1)
(define-constant ACHIEVEMENT_ECO_WARRIOR u2)
(define-constant ACHIEVEMENT_COMMUNITY_LEADER u3)
(define-constant ACHIEVEMENT_SUSTAINABILITY_CHAMPION u4)

;; Challenge Status
(define-constant CHALLENGE_ACTIVE u0)
(define-constant CHALLENGE_COMPLETED u1)
(define-constant CHALLENGE_EXPIRED u2)

;; Data Variables
(define-data-var user-counter uint u0)
(define-data-var challenge-counter uint u0)
(define-data-var global-footprint uint u0)
(define-data-var reward-multiplier uint u100) ;; 1.0x multiplier
(define-data-var daily-challenge-reward uint u10) ;; 10 points

;; Data Maps

;; User Profiles and Carbon Footprint
(define-map user-profiles
  principal
  {
    user-id: uint,
    username: (string-ascii 50),
    registration-date: uint,
    total-footprint: uint,
    annual-target: uint,
    current-year-emissions: uint,
    credits-owned: uint,
    credits-retired: uint,
    points-earned: uint,
    level: uint,
    streak-days: uint,
    last-activity: uint,
    privacy-setting: bool
  }
)

;; Carbon Footprint Activities
(define-map carbon-activities
  { user: principal, activity-id: uint }
  {
    activity-type: uint,
    date: uint,
    amount: uint,
    unit: (string-ascii 20),
    co2-equivalent: uint,
    description: (string-ascii 100),
    verified: bool,
    data-source: (string-ascii 50)
  }
)

;; Personal Goals and Targets
(define-map personal-goals
  { user: principal, goal-id: uint }
  {
    goal-type: (string-ascii 50),
    target-amount: uint,
    current-progress: uint,
    deadline: uint,
    reward-points: uint,
    completed: bool,
    created-date: uint
  }
)

;; Community Challenges
(define-map community-challenges
  uint ;; challenge-id
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    challenge-type: (string-ascii 50),
    target-metric: uint,
    reward-pool: uint,
    participants: uint,
    start-date: uint,
    end-date: uint,
    status: uint,
    creator: principal
  }
)

;; Challenge Participation
(define-map challenge-participation
  { challenge-id: uint, user: principal }
  {
    joined-date: uint,
    progress: uint,
    completed: bool,
    reward-earned: uint,
    final-rank: uint
  }
)

;; Achievements and Badges
(define-map user-achievements
  { user: principal, achievement-type: uint }
  {
    earned-date: uint,
    points-awarded: uint,
    description: (string-ascii 100),
    rarity-level: uint
  }
)

;; Offset Purchases and Retirement
(define-map offset-transactions
  { user: principal, transaction-id: uint }
  {
    transaction-type: (string-ascii 20), ;; "purchase" or "retirement"
    amount: uint,
    cost: uint,
    project-type: uint,
    vintage: uint,
    transaction-date: uint,
    certificate-hash: (string-ascii 64),
    reason: (string-ascii 100)
  }
)

;; Social Features
(define-map user-connections
  { user: principal, friend: principal }
  {
    connection-date: uint,
    connection-type: (string-ascii 20), ;; "friend", "following", etc.
    shared-challenges: uint
  }
)

;; Leaderboards
(define-map monthly-leaderboard
  { month: uint, rank: uint }
  {
    user: principal,
    score: uint,
    footprint-reduction: uint,
    offsets-purchased: uint
  }
)

;; Impact Tracking
(define-map impact-metrics
  { user: principal, metric-type: (string-ascii 30) }
  {
    total-value: uint,
    monthly-values: (list 12 uint),
    last-updated: uint,
    trend-direction: int, ;; positive = improving, negative = worsening
    benchmark-comparison: int
  }
)

;; Activity counters
(define-data-var activity-counter uint u0)
(define-data-var goal-counter uint u0)
(define-data-var transaction-counter uint u0)

;; Public Functions

;; Register new user
(define-public (register-user (username (string-ascii 50)) (annual-target uint))
  (let (
    (user-id (+ (var-get user-counter) u1))
    )
    ;; Check if user already exists
    (asserts! (is-none (map-get? user-profiles tx-sender)) ERR_ALREADY_PARTICIPATED)
    (asserts! (> (len username) u0) ERR_INVALID_FOOTPRINT)
    
    ;; Create user profile
    (map-set user-profiles tx-sender {
      user-id: user-id,
      username: username,
      registration-date: stacks-block-height,
      total-footprint: u0,
      annual-target: annual-target,
      current-year-emissions: u0,
      credits-owned: u0,
      credits-retired: u0,
      points-earned: u0,
      level: u1,
      streak-days: u0,
      last-activity: stacks-block-height,
      privacy-setting: false
    })
    
    ;; Update counter
    (var-set user-counter user-id)
    
    ;; Award registration achievement
    (try! (award-achievement tx-sender ACHIEVEMENT_FIRST_OFFSET u50 "Welcome to the carbon neutral journey!"))
    
    (ok user-id)
  )
)

;; Log carbon footprint activity
(define-public (log-carbon-activity
  (activity-type uint)
  (amount uint)
  (unit (string-ascii 20))
  (co2-equivalent uint)
  (description (string-ascii 100))
  (data-source (string-ascii 50))
  )
  (let (
    (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_USER_NOT_FOUND))
    (activity-id (+ (var-get activity-counter) u1))
    )
    ;; Validate activity type
    (asserts! (<= activity-type ACTIVITY_WASTE) ERR_INVALID_ACTIVITY)
    (asserts! (> co2-equivalent u0) ERR_INVALID_FOOTPRINT)
    
    ;; Record activity
    (map-set carbon-activities { user: tx-sender, activity-id: activity-id } {
      activity-type: activity-type,
      date: stacks-block-height,
      amount: amount,
      unit: unit,
      co2-equivalent: co2-equivalent,
      description: description,
      verified: false,
      data-source: data-source
    })
    
    ;; Update user footprint
    (map-set user-profiles tx-sender (merge user-profile {
      total-footprint: (+ (get total-footprint user-profile) co2-equivalent),
      current-year-emissions: (+ (get current-year-emissions user-profile) co2-equivalent),
      last-activity: stacks-block-height
    }))
    
    ;; Update global footprint
    (var-set global-footprint (+ (var-get global-footprint) co2-equivalent))
    
    ;; Update counter
    (var-set activity-counter activity-id)
    
    ;; Award points for tracking
    (try! (award-points tx-sender u5))
    
    (ok activity-id)
  )
)

;; Set personal carbon reduction goal
(define-public (set-personal-goal
  (goal-type (string-ascii 50))
  (target-amount uint)
  (deadline uint)
  (reward-points uint)
  )
  (let (
    (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_USER_NOT_FOUND))
    (goal-id (+ (var-get goal-counter) u1))
    )
    ;; Validate goal parameters
    (asserts! (> target-amount u0) ERR_INVALID_FOOTPRINT)
    (asserts! (> deadline stacks-block-height) ERR_INVALID_FOOTPRINT)
    
    ;; Create goal
    (map-set personal-goals { user: tx-sender, goal-id: goal-id } {
      goal-type: goal-type,
      target-amount: target-amount,
      current-progress: u0,
      deadline: deadline,
      reward-points: reward-points,
      completed: false,
      created-date: stacks-block-height
    })
    
    ;; Update counter
    (var-set goal-counter goal-id)
    
    (ok goal-id)
  )
)

;; Purchase carbon offsets
(define-public (purchase-offsets
  (amount uint)
  (cost uint)
  (project-type uint)
  (vintage uint)
  (reason (string-ascii 100))
  )
  (let (
    (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_USER_NOT_FOUND))
    (transaction-id (+ (var-get transaction-counter) u1))
    )
    ;; Validate purchase parameters
    (asserts! (> amount u0) ERR_INVALID_FOOTPRINT)
    (asserts! (> cost u0) ERR_INVALID_FOOTPRINT)
    
    ;; Record transaction
    (map-set offset-transactions { user: tx-sender, transaction-id: transaction-id } {
      transaction-type: "purchase",
      amount: amount,
      cost: cost,
      project-type: project-type,
      vintage: vintage,
      transaction-date: stacks-block-height,
      certificate-hash: "",
      reason: reason
    })
    
    ;; Update user credits
    (map-set user-profiles tx-sender (merge user-profile {
      credits-owned: (+ (get credits-owned user-profile) amount),
      last-activity: stacks-block-height
    }))
    
    ;; Update counter
    (var-set transaction-counter transaction-id)
    
    ;; Award points for purchase
    (try! (award-points tx-sender (* amount u2)))
    
    ;; Check for achievements
    (if (is-eq (get credits-owned user-profile) u0)
      (try! (award-achievement tx-sender ACHIEVEMENT_FIRST_OFFSET u100 "First carbon offset purchase!"))
      true
    )
    
    (ok transaction-id)
  )
)

;; Retire carbon offsets
(define-public (retire-offsets
  (amount uint)
  (reason (string-ascii 100))
  (certificate-hash (string-ascii 64))
  )
  (let (
    (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_USER_NOT_FOUND))
    (transaction-id (+ (var-get transaction-counter) u1))
    )
    ;; Check sufficient credits
    (asserts! (>= (get credits-owned user-profile) amount) ERR_INSUFFICIENT_CREDITS)
    (asserts! (> amount u0) ERR_INVALID_FOOTPRINT)
    
    ;; Record retirement transaction
    (map-set offset-transactions { user: tx-sender, transaction-id: transaction-id } {
      transaction-type: "retirement",
      amount: amount,
      cost: u0,
      project-type: u0,
      vintage: u0,
      transaction-date: stacks-block-height,
      certificate-hash: certificate-hash,
      reason: reason
    })
    
    ;; Update user credits
    (map-set user-profiles tx-sender (merge user-profile {
      credits-owned: (- (get credits-owned user-profile) amount),
      credits-retired: (+ (get credits-retired user-profile) amount),
      last-activity: stacks-block-height
    }))
    
    ;; Update counter
    (var-set transaction-counter transaction-id)
    
    ;; Award points for retirement
    (try! (award-points tx-sender (* amount u3)))
    
    ;; Check for carbon neutral achievement
    (if (>= (get credits-retired user-profile) (get current-year-emissions user-profile))
      (try! (award-achievement tx-sender ACHIEVEMENT_CARBON_NEUTRAL u500 "Carbon neutral for the year!"))
      true
    )
    
    (ok transaction-id)
  )
)

;; Create community challenge
(define-public (create-community-challenge
  (title (string-ascii 100))
  (description (string-ascii 300))
  (challenge-type (string-ascii 50))
  (target-metric uint)
  (reward-pool uint)
  (duration-blocks uint)
  )
  (let (
    (challenge-id (+ (var-get challenge-counter) u1))
    )
    ;; Validate challenge parameters
    (asserts! (> target-metric u0) ERR_INVALID_FOOTPRINT)
    (asserts! (> reward-pool u0) ERR_INVALID_FOOTPRINT)
    (asserts! (> duration-blocks u0) ERR_INVALID_FOOTPRINT)
    
    ;; Create challenge
    (map-set community-challenges challenge-id {
      title: title,
      description: description,
      challenge-type: challenge-type,
      target-metric: target-metric,
      reward-pool: reward-pool,
      participants: u0,
      start-date: stacks-block-height,
      end-date: (+ stacks-block-height duration-blocks),
      status: CHALLENGE_ACTIVE,
      creator: tx-sender
    })
    
    ;; Update counter
    (var-set challenge-counter challenge-id)
    
    (ok challenge-id)
  )
)

;; Join community challenge
(define-public (join-community-challenge (challenge-id uint))
  (let (
    (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_USER_NOT_FOUND))
    (challenge (unwrap! (map-get? community-challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
    )
    ;; Check if challenge is active
    (asserts! (is-eq (get status challenge) CHALLENGE_ACTIVE) ERR_CHALLENGE_ENDED)
    (asserts! (< stacks-block-height (get end-date challenge)) ERR_CHALLENGE_ENDED)
    
    ;; Check if already participating
    (asserts! (is-none (map-get? challenge-participation { challenge-id: challenge-id, user: tx-sender }))
              ERR_ALREADY_PARTICIPATED)
    
    ;; Join challenge
    (map-set challenge-participation { challenge-id: challenge-id, user: tx-sender } {
      joined-date: stacks-block-height,
      progress: u0,
      completed: false,
      reward-earned: u0,
      final-rank: u0
    })
    
    ;; Update challenge participant count
    (map-set community-challenges challenge-id (merge challenge {
      participants: (+ (get participants challenge) u1)
    }))
    
    ;; Award joining points
    (try! (award-points tx-sender u20))
    
    (ok true)
  )
)

;; Administrative Functions

;; Award points to user
(define-public (award-points (user principal) (points uint))
  (let (
    (user-profile (unwrap! (map-get? user-profiles user) ERR_USER_NOT_FOUND))
    (new-points (+ (get points-earned user-profile) points))
    (new-level (calculate-user-level new-points))
    )
    ;; Update user points and level
    (map-set user-profiles user (merge user-profile {
      points-earned: new-points,
      level: new-level
    }))
    
    (ok true)
  )
)

;; Read-Only Functions

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

;; Get carbon activity
(define-read-only (get-carbon-activity (user principal) (activity-id uint))
  (map-get? carbon-activities { user: user, activity-id: activity-id })
)

;; Get personal goal
(define-read-only (get-personal-goal (user principal) (goal-id uint))
  (map-get? personal-goals { user: user, goal-id: goal-id })
)

;; Get community challenge
(define-read-only (get-community-challenge (challenge-id uint))
  (map-get? community-challenges challenge-id)
)

;; Get challenge participation
(define-read-only (get-challenge-participation (challenge-id uint) (user principal))
  (map-get? challenge-participation { challenge-id: challenge-id, user: user })
)

;; Get offset transaction
(define-read-only (get-offset-transaction (user principal) (transaction-id uint))
  (map-get? offset-transactions { user: user, transaction-id: transaction-id })
)

;; Get user achievement
(define-read-only (get-user-achievement (user principal) (achievement-type uint))
  (map-get? user-achievements { user: user, achievement-type: achievement-type })
)

;; Calculate carbon footprint for period
(define-read-only (calculate-period-footprint (user principal) (start-block uint) (end-block uint))
  ;; This would require iterating through activities in the period
  ;; For now, return current year emissions
  (match (map-get? user-profiles user)
    profile (some (get current-year-emissions profile))
    none
  )
)

;; Get global statistics
(define-read-only (get-global-stats)
  {
    total-users: (var-get user-counter),
    total-footprint: (var-get global-footprint),
    total-challenges: (var-get challenge-counter),
    reward-multiplier: (var-get reward-multiplier)
  }
)

;; Check if user is carbon neutral
(define-read-only (is-carbon-neutral (user principal))
  (match (map-get? user-profiles user)
    profile (>= (get credits-retired profile) (get current-year-emissions profile))
    false
  )
)

;; Private Functions

;; Award achievement to user
(define-private (award-achievement (user principal) (achievement-type uint) (points uint) (description (string-ascii 100)))
  (let (
    (existing-achievement (map-get? user-achievements { user: user, achievement-type: achievement-type }))
    )
    ;; Only award if not already earned
    (if (is-none existing-achievement)
      (begin
        (map-set user-achievements { user: user, achievement-type: achievement-type } {
          earned-date: stacks-block-height,
          points-awarded: points,
          description: description,
          rarity-level: u1
        })
        (award-points user points)
      )
      (ok true)
    )
  )
)

;; Calculate user level based on points
(define-private (calculate-user-level (points uint))
  (if (>= points u10000)
    u10
    (if (>= points u5000)
      u9
      (if (>= points u2500)
        u8
        (if (>= points u1000)
          u7
          (if (>= points u500)
            u6
            (if (>= points u250)
              u5
              (if (>= points u100)
                u4
                (if (>= points u50)
                  u3
                  (if (>= points u20)
                    u2
                    u1
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

