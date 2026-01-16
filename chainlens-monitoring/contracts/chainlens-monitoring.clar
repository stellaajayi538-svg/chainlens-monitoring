;; ChainLens - Decentralized Monitoring Infrastructure
;; A reputation-based monitoring node system with staking and metrics collection

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-data (err u105))

;; Minimum stake required for monitoring nodes (in microSTX)
(define-constant min-stake u1000000)

;; Data Variables
(define-data-var next-node-id uint u1)
(define-data-var next-metric-id uint u1)

;; Data Maps
(define-map monitoring-nodes
  { node-id: uint }
  {
    operator: principal,
    stake-amount: uint,
    reputation-score: uint,
    is-active: bool,
    metrics-reported: uint
  }
)

(define-map operator-nodes
  { operator: principal }
  { node-id: uint }
)

(define-map metrics
  { metric-id: uint }
  {
    node-id: uint,
    chain-name: (string-ascii 20),
    gas-consumed: uint,
    error-count: uint,
    transaction-count: uint,
    timestamp: uint,
    anomaly-detected: bool
  }
)

(define-map alert-triggers
  { contract-address: principal }
  {
    max-gas-threshold: uint,
    max-error-rate: uint,
    is-paused: bool,
    owner: principal
  }
)

;; Public Functions

;; Register a new monitoring node
(define-public (register-node (stake uint))
  (let
    (
      (node-id (var-get next-node-id))
      (operator tx-sender)
    )
    (asserts! (>= stake min-stake) err-insufficient-stake)
    (asserts! (is-none (map-get? operator-nodes { operator: operator })) err-already-exists)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
    
    ;; Register node
    (map-set monitoring-nodes
      { node-id: node-id }
      {
        operator: operator,
        stake-amount: stake,
        reputation-score: u100,
        is-active: true,
        metrics-reported: u0
      }
    )
    
    (map-set operator-nodes
      { operator: operator }
      { node-id: node-id }
    )
    
    (var-set next-node-id (+ node-id u1))
    (ok node-id)
  )
)

;; Report metrics from a monitoring node
(define-public (report-metrics 
  (chain (string-ascii 20))
  (gas uint)
  (errors uint)
  (txs uint)
  (anomaly bool))
  (let
    (
      (metric-id (var-get next-metric-id))
      (operator-data (unwrap! (map-get? operator-nodes { operator: tx-sender }) err-unauthorized))
      (node-id (get node-id operator-data))
      (node-data (unwrap! (map-get? monitoring-nodes { node-id: node-id }) err-not-found))
    )
    (asserts! (get is-active node-data) err-unauthorized)
    
    ;; Store metrics
    (map-set metrics
      { metric-id: metric-id }
      {
        node-id: node-id,
        chain-name: chain,
        gas-consumed: gas,
        error-count: errors,
        transaction-count: txs,
        timestamp: block-height,
        anomaly-detected: anomaly
      }
    )
    
    ;; Update node statistics
    (map-set monitoring-nodes
      { node-id: node-id }
      (merge node-data { metrics-reported: (+ (get metrics-reported node-data) u1) })
    )
    
    (var-set next-metric-id (+ metric-id u1))
    (ok metric-id)
  )
)

;; Configure alert triggers for a contract
(define-public (configure-alerts 
  (contract-addr principal)
  (gas-threshold uint)
  (error-threshold uint))
  (begin
    (map-set alert-triggers
      { contract-address: contract-addr }
      {
        max-gas-threshold: gas-threshold,
        max-error-rate: error-threshold,
        is-paused: false,
        owner: tx-sender
      }
    )
    (ok true)
  )
)

;; Pause contract (emergency response)
(define-public (pause-contract (contract-addr principal))
  (let
    (
      (alert-data (unwrap! (map-get? alert-triggers { contract-address: contract-addr }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner alert-data)) err-owner-only)
    
    (map-set alert-triggers
      { contract-address: contract-addr }
      (merge alert-data { is-paused: true })
    )
    (ok true)
  )
)

;; Unpause contract
(define-public (unpause-contract (contract-addr principal))
  (let
    (
      (alert-data (unwrap! (map-get? alert-triggers { contract-address: contract-addr }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner alert-data)) err-owner-only)
    
    (map-set alert-triggers
      { contract-address: contract-addr }
      (merge alert-data { is-paused: false })
    )
    (ok true)
  )
)

;; Update node reputation
(define-public (update-reputation (node-id uint) (new-score uint))
  (let
    (
      (node-data (unwrap! (map-get? monitoring-nodes { node-id: node-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set monitoring-nodes
      { node-id: node-id }
      (merge node-data { reputation-score: new-score })
    )
    (ok true)
  )
)

;; Withdraw stake (deactivates node)
(define-public (withdraw-stake)
  (let
    (
      (operator-data (unwrap! (map-get? operator-nodes { operator: tx-sender }) err-not-found))
      (node-id (get node-id operator-data))
      (node-data (unwrap! (map-get? monitoring-nodes { node-id: node-id }) err-not-found))
      (stake (get stake-amount node-data))
    )
    (asserts! (get is-active node-data) err-unauthorized)
    
    ;; Deactivate node
    (map-set monitoring-nodes
      { node-id: node-id }
      (merge node-data { is-active: false })
    )
    
    ;; Return stake
    (as-contract (stx-transfer? stake tx-sender (get operator node-data)))
  )
)

;; Read-only functions

(define-read-only (get-node-info (node-id uint))
  (map-get? monitoring-nodes { node-id: node-id })
)

(define-read-only (get-operator-node (operator principal))
  (map-get? operator-nodes { operator: operator })
)

(define-read-only (get-metric (metric-id uint))
  (map-get? metrics { metric-id: metric-id })
)

(define-read-only (get-alert-config (contract-addr principal))
  (map-get? alert-triggers { contract-address: contract-addr })
)

(define-read-only (get-min-stake)
  min-stake
)

(define-read-only (is-contract-paused (contract-addr principal))
  (match (map-get? alert-triggers { contract-address: contract-addr })
    alert-data (ok (get is-paused alert-data))
    err-not-found
  )
)