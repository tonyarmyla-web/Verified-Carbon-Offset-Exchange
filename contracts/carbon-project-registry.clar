;; Carbon Project Registry Contract
;; Verification and registration of carbon offset projects with satellite monitoring and IoT sensor data integration

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_PROJECT_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PROJECT_TYPE (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_UNAUTHORIZED (err u105))
(define-constant ERR_INVALID_COORDINATES (err u106))
(define-constant ERR_INVALID_AREA (err u107))
(define-constant ERR_PROJECT_INACTIVE (err u108))
(define-constant ERR_VERIFICATION_FAILED (err u109))

;; Project Status Types
(define-constant STATUS_PENDING u0)
(define-constant STATUS_VERIFIED u1)
(define-constant STATUS_ACTIVE u2)
(define-constant STATUS_SUSPENDED u3)
(define-constant STATUS_COMPLETED u4)

;; Project Types
(define-constant TYPE_REFORESTATION u0)
(define-constant TYPE_RENEWABLE_ENERGY u1)
(define-constant TYPE_METHANE_CAPTURE u2)
(define-constant TYPE_SOIL_CARBON u3)
(define-constant TYPE_OCEAN_CONSERVATION u4)

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var verification-threshold uint u3)
(define-data-var minimum-project-area uint u1000) ;; minimum square meters

;; Data Maps
(define-map projects
  uint
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    project-type: uint,
    status: uint,
    latitude: int,
    longitude: int,
    area-size: uint,
    estimated-credits: uint,
    actual-credits: uint,
    verification-count: uint,
    created-at: uint,
    updated-at: uint,
    certification-hash: (string-ascii 64),
    iot-sensor-id: (string-ascii 32),
    satellite-data-hash: (string-ascii 64)
  }
)

(define-map project-verifications
  { project-id: uint, verifier: principal }
  {
    verification-date: uint,
    verification-hash: (string-ascii 64),
    iot-data-verified: bool,
    satellite-data-verified: bool,
    overall-score: uint
  }
)

(define-map authorized-verifiers principal bool)

(define-map project-sensors
  uint
  {
    sensor-id: (string-ascii 32),
    sensor-type: (string-ascii 20),
    last-reading: uint,
    reading-value: uint,
    calibration-date: uint,
    active: bool
  }
)

(define-map project-satellite-data
  uint
  {
    satellite-provider: (string-ascii 50),
    image-hash: (string-ascii 64),
    analysis-date: uint,
    vegetation-index: uint,
    deforestation-detected: bool,
    carbon-sequestration-rate: uint
  }
)

;; Public Functions

;; Register a new carbon offset project
(define-public (register-project 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (project-type uint)
  (latitude int)
  (longitude int)
  (area-size uint)
  (estimated-credits uint)
  (iot-sensor-id (string-ascii 32))
  )
  (let (
    (project-id (+ (var-get project-counter) u1))
    )
    ;; Validate inputs
    (asserts! (<= project-type TYPE_OCEAN_CONSERVATION) ERR_INVALID_PROJECT_TYPE)
    (asserts! (>= area-size (var-get minimum-project-area)) ERR_INVALID_AREA)
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    
    ;; Create project entry
    (map-set projects project-id {
      owner: tx-sender,
      name: name,
      description: description,
      project-type: project-type,
      status: STATUS_PENDING,
      latitude: latitude,
      longitude: longitude,
      area-size: area-size,
      estimated-credits: estimated-credits,
      actual-credits: u0,
      verification-count: u0,
      created-at: stacks-block-height,
      updated-at: stacks-block-height,
      certification-hash: "",
      iot-sensor-id: iot-sensor-id,
      satellite-data-hash: ""
    })
    
    ;; Initialize sensor data
    (map-set project-sensors project-id {
      sensor-id: iot-sensor-id,
      sensor-type: "environmental",
      last-reading: u0,
      reading-value: u0,
      calibration-date: stacks-block-height,
      active: true
    })
    
    ;; Update counter
    (var-set project-counter project-id)
    
    (ok project-id)
  )
)

;; Update project status (only by authorized verifiers)
(define-public (update-project-status (project-id uint) (new-status uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    ;; Check if caller is authorized verifier
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (<= new-status STATUS_COMPLETED) ERR_INVALID_STATUS)
    
    ;; Update project status
    (map-set projects project-id (merge project {
      status: new-status,
      updated-at: stacks-block-height
    }))
    
    (ok true)
  )
)

;; Add IoT sensor data
(define-public (update-sensor-data 
  (project-id uint)
  (reading-value uint)
  (sensor-type (string-ascii 20))
  )
  (let (
    (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    (sensor (unwrap! (map-get? project-sensors project-id) ERR_PROJECT_NOT_FOUND))
    )
    ;; Check if project owner or authorized verifier
    (asserts! (or (is-eq tx-sender (get owner project)) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR_UNAUTHORIZED)
    
    ;; Update sensor data
    (map-set project-sensors project-id (merge sensor {
      sensor-type: sensor-type,
      last-reading: stacks-block-height,
      reading-value: reading-value
    }))
    
    (ok true)
  )
)

;; Add satellite verification data
(define-public (update-satellite-data
  (project-id uint)
  (provider (string-ascii 50))
  (image-hash (string-ascii 64))
  (vegetation-index uint)
  (deforestation-detected bool)
  (carbon-rate uint)
  )
  (let (
    (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    ;; Only authorized verifiers can update satellite data
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) ERR_UNAUTHORIZED)
    
    ;; Update satellite data
    (map-set project-satellite-data project-id {
      satellite-provider: provider,
      image-hash: image-hash,
      analysis-date: stacks-block-height,
      vegetation-index: vegetation-index,
      deforestation-detected: deforestation-detected,
      carbon-sequestration-rate: carbon-rate
    })
    
    ;; Update project with satellite hash
    (map-set projects project-id (merge project {
      satellite-data-hash: image-hash,
      updated-at: stacks-block-height
    }))
    
    (ok true)
  )
)

;; Verify project (by authorized verifier)
(define-public (verify-project 
  (project-id uint)
  (verification-hash (string-ascii 64))
  (iot-verified bool)
  (satellite-verified bool)
  (score uint)
  )
  (let (
    (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    (current-verifications (get verification-count project))
    )
    ;; Check if caller is authorized verifier
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (<= score u100) ERR_VERIFICATION_FAILED)
    
    ;; Record verification
    (map-set project-verifications { project-id: project-id, verifier: tx-sender } {
      verification-date: stacks-block-height,
      verification-hash: verification-hash,
      iot-data-verified: iot-verified,
      satellite-data-verified: satellite-verified,
      overall-score: score
    })
    
    ;; Update project verification count
    (map-set projects project-id (merge project {
      verification-count: (+ current-verifications u1),
      updated-at: stacks-block-height,
      certification-hash: verification-hash
    }))
    
    ;; Auto-approve if threshold met and score is good
    (if (and (>= (+ current-verifications u1) (var-get verification-threshold))
             (>= score u75))
        (map-set projects project-id (merge project {
          status: STATUS_VERIFIED,
          verification-count: (+ current-verifications u1),
          updated-at: stacks-block-height,
          certification-hash: verification-hash
        }))
        true
    )
    
    (ok true)
  )
)

;; Add authorized verifier (only contract owner)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

;; Remove authorized verifier (only contract owner)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Update verification threshold (only contract owner)
(define-public (set-verification-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (var-set verification-threshold new-threshold)
    (ok true)
  )
)

;; Read-Only Functions

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

;; Get project sensor data
(define-read-only (get-project-sensors (project-id uint))
  (map-get? project-sensors project-id)
)

;; Get project satellite data
(define-read-only (get-satellite-data (project-id uint))
  (map-get? project-satellite-data project-id)
)

;; Get verification details
(define-read-only (get-verification (project-id uint) (verifier principal))
  (map-get? project-verifications { project-id: project-id, verifier: verifier })
)

;; Check if verifier is authorized
(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

;; Get current project counter
(define-read-only (get-project-count)
  (var-get project-counter)
)

;; Get verification threshold
(define-read-only (get-verification-threshold)
  (var-get verification-threshold)
)

;; Get projects by owner
(define-read-only (get-projects-by-status (status uint))
  ;; This would require additional implementation to filter by status
  ;; For now, returning the status for validation
  status
)

;; Check project eligibility for credit issuance
(define-read-only (is-project-eligible (project-id uint))
  (match (map-get? projects project-id)
    project (and 
              (is-eq (get status project) STATUS_VERIFIED)
              (>= (get verification-count project) (var-get verification-threshold))
            )
    false
  )
)

