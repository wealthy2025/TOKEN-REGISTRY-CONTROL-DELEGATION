;; ================================================================================================
;; TOKEN REGISTRY CONTROL DELEGATION - PRODUCTION SMART CONTRACT
;; ================================================================================================
;; 
;; Advanced token registration and delegation management system for blockchain-based assets
;; Implements comprehensive ownership control, permission delegation, and metadata management
;; Designed for enterprise-level token lifecycle management with granular access controls
;; 
;; Features:
;; - Decentralized token registration with immutable ownership records
;; - Hierarchical delegation system for controlled access management  
;; - Comprehensive metadata storage with validation and integrity checks
;; - Role-based permission system with fine-grained access controls
;; - Production-ready error handling and validation frameworks
;;
;; ================================================================================================

;; ================================================================================================
;; SYSTEM CONFIGURATION & AUTHORITY MANAGEMENT
;; ================================================================================================

;; Primary contract administrator with elevated system privileges
(define-constant contract-controller-principal tx-sender)

;; ================================================================================================
;; COMPREHENSIVE ERROR CODE DEFINITIONS
;; ================================================================================================

;; Registry operation error codes with descriptive status indicators
(define-constant token-record-missing-error (err u501))
(define-constant registry-entry-exists-error (err u502))
(define-constant invalid-metadata-format-error (err u503))
(define-constant token-size-limit-exceeded-error (err u504))
(define-constant delegation-unauthorized-error (err u505))
(define-constant ownership-mismatch-error (err u506))
(define-constant system-admin-required-error (err u500))
(define-constant restricted-token-access-error (err u507))
(define-constant tag-format-validation-error (err u508))

;; ================================================================================================
;; GLOBAL STATE VARIABLES & COUNTERS
;; ================================================================================================

;; Sequential token identifier counter for unique registry entries
(define-data-var token-registry-sequence uint u0)

;; ================================================================================================
;; PRIMARY DATA STORAGE STRUCTURES
;; ================================================================================================

;; Main token registry repository with comprehensive token metadata
(define-map token-control-registry
  { token-record-id: uint }
  {
    token-identifier-name: (string-ascii 64),
    controlling-owner: principal,
    token-size-bytes: uint,
    creation-block-number: uint,
    detailed-token-description: (string-ascii 128),
    classification-labels: (list 10 (string-ascii 32))
  }
)

;; Token delegation and access control permission mapping
(define-map delegation-authorization-map
  { token-record-id: uint, delegated-principal: principal }
  { has-access-permission: bool }
)

;; ================================================================================================
;; INTERNAL VALIDATION & UTILITY FUNCTION LIBRARY  
;; ================================================================================================

;; Classification label format validation with strict character constraints
(define-private (validate-label-format (classification-label (string-ascii 32)))
  (and
    ;; Ensure label is not empty
    (> (len classification-label) u0)
    ;; Enforce maximum length constraint for consistency
    (< (len classification-label) u33)
  )
)

;; Comprehensive label collection validation ensuring all entries meet format requirements
(define-private (validate-label-collection-integrity (label-collection (list 10 (string-ascii 32))))
  (and
    ;; Collection must contain at least one label
    (> (len label-collection) u0)
    ;; Collection cannot exceed maximum capacity
    (<= (len label-collection) u10)
    ;; All labels must pass individual format validation
    (is-eq 
      (len (filter validate-label-format label-collection)) 
      (len label-collection)
    )
  )
)

;; Token registry existence verification utility function
(define-private (verify-token-exists-in-registry (token-record-id uint))
  (is-some (map-get? token-control-registry { token-record-id: token-record-id }))
)

;; Safe token size retrieval with fallback to zero for missing entries
(define-private (get-token-size-safe (token-record-id uint))
  (default-to u0
    (get token-size-bytes
      (map-get? token-control-registry { token-record-id: token-record-id })
    )
  )
)

;; Ownership verification ensuring caller has legitimate control over token
(define-private (verify-ownership-authority (token-record-id uint) (claiming-principal principal))
  (match (map-get? token-control-registry { token-record-id: token-record-id })
    token-data (is-eq (get controlling-owner token-data) claiming-principal)
    false
  )
)

;; ================================================================================================
;; TOKEN REGISTRATION & MANAGEMENT PUBLIC FUNCTIONS
;; ================================================================================================

;; Primary token registration function with comprehensive metadata validation
(define-public (create-token-registry-entry
  (token-identifier-name (string-ascii 64))
  (token-size-bytes uint)
  (detailed-token-description (string-ascii 128))
  (classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Generate unique sequential identifier for new token
      (next-token-id (+ (var-get token-registry-sequence) u1))
    )

    ;; ========== INPUT VALIDATION LAYER ==========

    ;; Token name validation - must be non-empty and within length limits
    (asserts! (> (len token-identifier-name) u0) invalid-metadata-format-error)
    (asserts! (< (len token-identifier-name) u65) invalid-metadata-format-error)

    ;; Token size validation - must be positive and within reasonable bounds
    (asserts! (> token-size-bytes u0) token-size-limit-exceeded-error)
    (asserts! (< token-size-bytes u1000000000) token-size-limit-exceeded-error)

    ;; Description validation - must be meaningful and properly sized
    (asserts! (> (len detailed-token-description) u0) invalid-metadata-format-error)
    (asserts! (< (len detailed-token-description) u129) invalid-metadata-format-error)

    ;; Classification labels validation using comprehensive integrity check
    (asserts! (validate-label-collection-integrity classification-labels) tag-format-validation-error)

    ;; ========== REGISTRY ENTRY CREATION ==========

    ;; Store complete token metadata in primary registry
    (map-insert token-control-registry
      { token-record-id: next-token-id }
      {
        token-identifier-name: token-identifier-name,
        controlling-owner: tx-sender,
        token-size-bytes: token-size-bytes,
        creation-block-number: block-height,
        detailed-token-description: detailed-token-description,
        classification-labels: classification-labels
      }
    )

    ;; ========== DELEGATION INITIALIZATION ==========

    ;; Grant automatic access permission to token creator
    (map-insert delegation-authorization-map
      { token-record-id: next-token-id, delegated-principal: tx-sender }
      { has-access-permission: true }
    )

    ;; ========== STATE UPDATE & RESPONSE ==========

    ;; Increment global token counter for next registration
    (var-set token-registry-sequence next-token-id)

    ;; Return success with newly created token ID
    (ok next-token-id)
  )
)

;; Comprehensive token metadata update function with ownership verification
(define-public (modify-token-registry-metadata
  (token-record-id uint)
  (updated-token-name (string-ascii 64))
  (updated-size-bytes uint)
  (updated-description (string-ascii 128))
  (updated-labels (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Retrieve existing token data for validation and merging
      (existing-token-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
    )

    ;; ========== AUTHORIZATION & EXISTENCE VALIDATION ==========

    ;; Verify token exists in registry before proceeding
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Ensure caller has ownership authority for modification
    (asserts! (is-eq (get controlling-owner existing-token-data) tx-sender) ownership-mismatch-error)

    ;; ========== UPDATED METADATA VALIDATION ==========

    ;; Validate updated token name format and constraints
    (asserts! (> (len updated-token-name) u0) invalid-metadata-format-error)
    (asserts! (< (len updated-token-name) u65) invalid-metadata-format-error)

    ;; Validate updated size parameters within acceptable ranges
    (asserts! (> updated-size-bytes u0) token-size-limit-exceeded-error)
    (asserts! (< updated-size-bytes u1000000000) token-size-limit-exceeded-error)

    ;; Validate updated description meets format requirements
    (asserts! (> (len updated-description) u0) invalid-metadata-format-error)
    (asserts! (< (len updated-description) u129) invalid-metadata-format-error)

    ;; Validate updated classification labels using integrity checker
    (asserts! (validate-label-collection-integrity updated-labels) tag-format-validation-error)

    ;; ========== METADATA UPDATE EXECUTION ==========

    ;; Apply comprehensive metadata update while preserving ownership and creation data
    (map-set token-control-registry
      { token-record-id: token-record-id }
      (merge existing-token-data {
        token-identifier-name: updated-token-name,
        token-size-bytes: updated-size-bytes,
        detailed-token-description: updated-description,
        classification-labels: updated-labels
      })
    )

    ;; Return successful operation confirmation
    (ok true)
  )
)

;; Secure ownership transfer mechanism with comprehensive validation protocols
(define-public (transfer-token-ownership (token-record-id uint) (new-controlling-owner principal))
  (let
    (
      ;; Retrieve current token data for ownership verification
      (current-token-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
    )

    ;; ========== OWNERSHIP TRANSFER VALIDATION ==========

    ;; Confirm token exists in registry before transfer
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Verify current caller has legitimate ownership authority
    (asserts! (is-eq (get controlling-owner current-token-data) tx-sender) ownership-mismatch-error)

    ;; ========== OWNERSHIP TRANSFER EXECUTION ==========

    ;; Execute secure ownership transfer with updated principal
    (map-set token-control-registry
      { token-record-id: token-record-id }
      (merge current-token-data { controlling-owner: new-controlling-owner })
    )

    ;; Return successful transfer confirmation
    (ok true)
  )
)

;; Permanent token removal function with irreversible deletion capability
(define-public (delete-token-from-registry (token-record-id uint))
  (let
    (
      ;; Retrieve token data for final ownership verification
      (token-to-delete 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
    )

    ;; ========== DELETION AUTHORIZATION ==========

    ;; Ensure token exists before attempting deletion
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Verify caller has ownership authority for deletion
    (asserts! (is-eq (get controlling-owner token-to-delete) tx-sender) ownership-mismatch-error)

    ;; ========== PERMANENT DELETION EXECUTION ==========

    ;; Execute irreversible token removal from registry
    (map-delete token-control-registry { token-record-id: token-record-id })

    ;; Return successful deletion confirmation
    (ok true)
  )
)

;; ================================================================================================
;; DELEGATION & ACCESS CONTROL MANAGEMENT FUNCTIONS
;; ================================================================================================

;; Advanced delegation permission granting with comprehensive access control
(define-public (grant-delegation-access (token-record-id uint) (target-principal principal))
  (let
    (
      ;; Retrieve token data for ownership verification
      (token-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
    )

    ;; ========== DELEGATION AUTHORIZATION ==========

    ;; Verify token exists in registry
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Ensure only token owner can grant delegation permissions
    (asserts! (is-eq (get controlling-owner token-data) tx-sender) ownership-mismatch-error)

    ;; ========== DELEGATION PERMISSION GRANT ==========

    ;; Create or update delegation permission for target principal
    (map-set delegation-authorization-map
      { token-record-id: token-record-id, delegated-principal: target-principal }
      { has-access-permission: true }
    )

    ;; Return successful delegation grant confirmation
    (ok true)
  )
)

;; Delegation permission revocation with secure access removal
(define-public (revoke-delegation-access (token-record-id uint) (target-principal principal))
  (let
    (
      ;; Retrieve token data for ownership verification  
      (token-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
    )

    ;; ========== REVOCATION AUTHORIZATION ==========

    ;; Verify token exists in registry
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Ensure only token owner can revoke delegation permissions
    (asserts! (is-eq (get controlling-owner token-data) tx-sender) ownership-mismatch-error)

    ;; ========== DELEGATION PERMISSION REVOCATION ==========

    ;; Remove delegation permission for target principal
    (map-delete delegation-authorization-map
      { token-record-id: token-record-id, delegated-principal: target-principal }
    )

    ;; Return successful delegation revocation confirmation
    (ok true)
  )
)

;; ================================================================================================
;; READ-ONLY DATA RETRIEVAL & QUERY FUNCTIONS
;; ================================================================================================

;; Comprehensive token information retrieval with access control enforcement
(define-read-only (get-token-registry-details (token-record-id uint))
  (let
    (
      ;; Retrieve complete token registry data
      (token-registry-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
      ;; Check delegation authorization status for calling principal
      (delegation-status 
        (default-to false
          (get has-access-permission
            (map-get? delegation-authorization-map 
              { token-record-id: token-record-id, delegated-principal: tx-sender }
            )
          )
        )
      )
    )

    ;; ========== ACCESS CONTROL VERIFICATION ==========

    ;; Confirm token exists in registry
    (asserts! (verify-token-exists-in-registry token-record-id) token-record-missing-error)

    ;; Enforce access control - owner or delegated principals only
    (asserts! 
      (or delegation-status (is-eq (get controlling-owner token-registry-data) tx-sender)) 
      restricted-token-access-error
    )

    ;; ========== COMPREHENSIVE DATA RESPONSE ==========

    ;; Return complete token information with all metadata fields
    (ok {
      token-identifier-name: (get token-identifier-name token-registry-data),
      controlling-owner: (get controlling-owner token-registry-data),
      token-size-bytes: (get token-size-bytes token-registry-data),
      creation-block-number: (get creation-block-number token-registry-data),
      detailed-token-description: (get detailed-token-description token-registry-data),
      classification-labels: (get classification-labels token-registry-data)
    })
  )
)

;; System-wide registry statistics and administrative information retrieval
(define-read-only (get-registry-system-overview)
  (ok {
    total-registered-tokens: (var-get token-registry-sequence),
    contract-controller-authority: contract-controller-principal
  })
)

;; Token ownership verification utility for external queries
(define-read-only (verify-token-ownership (token-record-id uint))
  (match (map-get? token-control-registry { token-record-id: token-record-id })
    token-data (ok (get controlling-owner token-data))
    token-record-missing-error
  )
)

;; Comprehensive delegation authorization status checker with detailed permissions
(define-read-only (check-delegation-permissions (token-record-id uint) (queried-principal principal))
  (let
    (
      ;; Retrieve token data for ownership comparison
      (token-data 
        (unwrap! 
          (map-get? token-control-registry { token-record-id: token-record-id })
          token-record-missing-error
        )
      )
      ;; Check explicit delegation permission status
      (explicit-delegation 
        (default-to false
          (get has-access-permission
            (map-get? delegation-authorization-map 
              { token-record-id: token-record-id, delegated-principal: queried-principal }
            )
          )
        )
      )
    )

    ;; ========== COMPREHENSIVE PERMISSION ANALYSIS ==========

    ;; Return detailed permission breakdown with multiple access indicators
    (ok {
      has-explicit-delegation: explicit-delegation,
      is-token-owner: (is-eq (get controlling-owner token-data) queried-principal),
      has-full-access: (or explicit-delegation (is-eq (get controlling-owner token-data) queried-principal))
    })
  )
)

;; ================================================================================================
;; ADVANCED UTILITY & HELPER FUNCTIONS
;; ================================================================================================

;; Token metadata summary retrieval for lightweight queries
(define-read-only (get-token-metadata-summary (token-record-id uint))
  (match (map-get? token-control-registry { token-record-id: token-record-id })
    token-data (ok {
      name: (get token-identifier-name token-data),
      owner: (get controlling-owner token-data),
      size: (get token-size-bytes token-data),
      created: (get creation-block-number token-data)
    })
    token-record-missing-error
  )
)

;; Registry health check function for system monitoring
(define-read-only (perform-registry-health-check)
  (ok {
    registry-operational: true,
    current-sequence: (var-get token-registry-sequence),
    controller: contract-controller-principal,
    block-height: block-height
  })
)