;; OmniStreaming - Decentralized Music Platform
;; Core smart contract for song registration, streaming, and royalty distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-percentage (err u105))

;; Platform fee (5% = 500 basis points)
(define-constant platform-fee-bps u500)
(define-constant basis-points u10000)

;; Data Variables
(define-data-var next-song-id uint u1)
(define-data-var platform-balance uint u0)

;; Data Maps
(define-map songs
  uint
  {
    artist: principal,
    title: (string-ascii 100),
    ipfs-hash: (string-ascii 64),
    base-price: uint,
    total-streams: uint,
    reputation-score: uint,
    active: bool
  }
)

(define-map song-collaborators
  { song-id: uint, collaborator: principal }
  { royalty-percentage: uint }
)

(define-map artist-stats
  principal
  {
    total-songs: uint,
    total-streams: uint,
    total-earnings: uint,
    reputation: uint
  }
)

(define-map listener-stats
  principal
  {
    total-streams: uint,
    total-spent: uint
  }
)

;; Read-only functions
(define-read-only (get-song (song-id uint))
  (map-get? songs song-id)
)

(define-read-only (get-artist-stats (artist principal))
  (default-to
    { total-songs: u0, total-streams: u0, total-earnings: u0, reputation: u100 }
    (map-get? artist-stats artist)
  )
)

(define-read-only (get-listener-stats (listener principal))
  (default-to
    { total-streams: u0, total-spent: u0 }
    (map-get? listener-stats listener)
  )
)

(define-read-only (get-collaborator-share (song-id uint) (collaborator principal))
  (map-get? song-collaborators { song-id: song-id, collaborator: collaborator })
)

(define-read-only (calculate-stream-price (song-id uint))
  (match (map-get? songs song-id)
    song
    (let
      (
        (base-price (get base-price song))
        (streams (get total-streams song))
        (reputation (get reputation-score song))
        ;; Dynamic pricing: increase 1% per 1000 streams, adjust by reputation
        (demand-multiplier (+ u100 (/ streams u1000)))
        (reputation-multiplier (/ reputation u100))
        (adjusted-price (/ (* (* base-price demand-multiplier) reputation-multiplier) u10000))
      )
      (ok adjusted-price)
    )
    err-not-found
  )
)

(define-read-only (get-platform-balance)
  (var-get platform-balance)
)

;; Private functions
(define-private (update-artist-stats (artist principal) (earnings uint))
  (let
    (
      (current-stats (get-artist-stats artist))
      (new-total-earnings (+ (get total-earnings current-stats) earnings))
      (new-total-streams (+ (get total-streams current-stats) u1))
      ;; Reputation increases with activity (capped at 200)
      (new-reputation (if (< (get reputation current-stats) u200)
        (+ (get reputation current-stats) u1)
        u200
      ))
    )
    (map-set artist-stats artist
      {
        total-songs: (get total-songs current-stats),
        total-streams: new-total-streams,
        total-earnings: new-total-earnings,
        reputation: new-reputation
      }
    )
  )
)

(define-private (update-listener-stats (listener principal) (amount uint))
  (let
    (
      (current-stats (get-listener-stats listener))
    )
    (map-set listener-stats listener
      {
        total-streams: (+ (get total-streams current-stats) u1),
        total-spent: (+ (get total-spent current-stats) amount)
      }
    )
  )
)

;; Public functions
(define-public (register-song (title (string-ascii 100)) (ipfs-hash (string-ascii 64)) (base-price uint))
  (let
    (
      (song-id (var-get next-song-id))
      (current-stats (get-artist-stats tx-sender))
    )
    (asserts! (> base-price u0) err-invalid-percentage)
    (map-set songs song-id
      {
        artist: tx-sender,
        title: title,
        ipfs-hash: ipfs-hash,
        base-price: base-price,
        total-streams: u0,
        reputation-score: u100,
        active: true
      }
    )
    (map-set artist-stats tx-sender
      {
        total-songs: (+ (get total-songs current-stats) u1),
        total-streams: (get total-streams current-stats),
        total-earnings: (get total-earnings current-stats),
        reputation: (get reputation current-stats)
      }
    )
    (var-set next-song-id (+ song-id u1))
    (ok song-id)
  )
)

(define-public (add-collaborator (song-id uint) (collaborator principal) (royalty-percentage uint))
  (let
    (
      (song (unwrap! (map-get? songs song-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get artist song)) err-unauthorized)
    (asserts! (<= royalty-percentage u10000) err-invalid-percentage)
    (ok (map-set song-collaborators
      { song-id: song-id, collaborator: collaborator }
      { royalty-percentage: royalty-percentage }
    ))
  )
)

(define-public (stream-song (song-id uint))
  (let
    (
      (song (unwrap! (map-get? songs song-id) err-not-found))
      (stream-price (unwrap! (calculate-stream-price song-id) err-not-found))
      (platform-fee (/ (* stream-price platform-fee-bps) basis-points))
      (artist-payment (- stream-price platform-fee))
      (artist (get artist song))
    )
    (asserts! (get active song) err-unauthorized)
    
    ;; Transfer payment from listener to artist
    (try! (stx-transfer? artist-payment tx-sender artist))
    
    ;; Update platform balance
    (var-set platform-balance (+ (var-get platform-balance) platform-fee))
    
    ;; Update song stats
    (map-set songs song-id
      (merge song { total-streams: (+ (get total-streams song) u1) })
    )
    
    ;; Update artist and listener stats
    (update-artist-stats artist artist-payment)
    (update-listener-stats tx-sender stream-price)
    
    (ok stream-price)
  )
)

(define-public (stream-song-with-split (song-id uint))
  (let
    (
      (song (unwrap! (map-get? songs song-id) err-not-found))
      (stream-price (unwrap! (calculate-stream-price song-id) err-not-found))
      (platform-fee (/ (* stream-price platform-fee-bps) basis-points))
      (net-payment (- stream-price platform-fee))
      (artist (get artist song))
    )
    (asserts! (get active song) err-unauthorized)
    
    ;; Transfer full payment from listener
    (try! (stx-transfer? stream-price tx-sender (as-contract tx-sender)))
    
    ;; Pay primary artist (assuming 80% if no collaborators specified)
    (try! (as-contract (stx-transfer? (/ (* net-payment u8000) basis-points) tx-sender artist)))
    
    ;; Update platform balance
    (var-set platform-balance (+ (var-get platform-balance) platform-fee))
    
    ;; Update song stats
    (map-set songs song-id
      (merge song { total-streams: (+ (get total-streams song) u1) })
    )
    
    ;; Update stats
    (update-artist-stats artist (/ (* net-payment u8000) basis-points))
    (update-listener-stats tx-sender stream-price)
    
    (ok stream-price)
  )
)

(define-public (toggle-song-status (song-id uint))
  (let
    (
      (song (unwrap! (map-get? songs song-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get artist song)) err-unauthorized)
    (ok (map-set songs song-id
      (merge song { active: (not (get active song)) })
    ))
  )
)

(define-public (update-base-price (song-id uint) (new-price uint))
  (let
    (
      (song (unwrap! (map-get? songs song-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get artist song)) err-unauthorized)
    (asserts! (> new-price u0) err-invalid-percentage)
    (ok (map-set songs song-id
      (merge song { base-price: new-price })
    ))
  )
)

;; Admin functions
(define-public (withdraw-platform-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get platform-balance)) err-insufficient-funds)
    (var-set platform-balance (- (var-get platform-balance) amount))
    (as-contract (stx-transfer? amount tx-sender contract-owner))
  )
)
