;; Agricultural Weather Monitoring Smart Contract
;; Farm weather system with microclimate tracking, frost warnings, 
;; irrigation scheduling, and crop protection alerts

;; Constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-STATION (err u101))
(define-constant ERR-INVALID-READING (err u102))
(define-constant ERR-STATION-EXISTS (err u103))
(define-constant ERR-INVALID-THRESHOLD (err u104))

;; Data Variables
(define-data-var station-counter uint u0)

;; Maps
(define-map weather-stations uint {
  name: (string-ascii 50),
  location: (string-ascii 100),
  owner: principal,
  active: bool,
  installed-at: uint
})

(define-map weather-readings uint {
  station-id: uint,
  timestamp: uint,
  temperature: int,     ;; Celsius * 100 (e.g., 2550 = 25.50C)
  humidity: uint,       ;; Percentage * 100 (e.g., 7500 = 75.00%)
  soil-moisture: uint,  ;; Percentage * 100
  wind-speed: uint,     ;; km/h * 100
  precipitation: uint   ;; mm * 100 (e.g., 250 = 2.50mm)
})

(define-map frost-alerts uint {
  station-id: uint,
  alert-time: uint,
  severity: (string-ascii 20),
  active: bool
})

(define-map irrigation-schedules uint {
  station-id: uint,
  start-time: uint,
  duration: uint,       ;; minutes
  frequency: uint,      ;; hours between irrigations
  active: bool
})

(define-map crop-alerts uint {
  station-id: uint,
  alert-type: (string-ascii 30),
  created-at: uint,
  message: (string-ascii 200),
  active: bool
})

;; Alert thresholds
(define-map alert-thresholds uint {
  frost-temp: int,          ;; Temperature threshold for frost (Celsius * 100)
  high-humidity: uint,      ;; High humidity threshold (percentage * 100)
  low-soil-moisture: uint,  ;; Low soil moisture threshold (percentage * 100)
  high-wind-speed: uint     ;; High wind speed threshold (km/h * 100)
})

;; Private Functions
(define-private (is-station-owner (station-id uint) (user principal))
  (match (map-get? weather-stations station-id)
    station (is-eq (get owner station) user)
    false
  )
)

(define-private (station-exists? (station-id uint))
  (is-some (map-get? weather-stations station-id))
)

(define-private (check-frost-conditions (station-id uint) (temperature int))
  (match (map-get? alert-thresholds station-id)
    thresholds (<= temperature (get frost-temp thresholds))
    (<= temperature -200) ;; Default -2C if no thresholds set
  )
)

;; Public Functions

;; Register a new weather station
(define-public (register-station (name (string-ascii 50)) (location (string-ascii 100)))
  (let (
    (new-id (+ (var-get station-counter) u1))
  )
    (asserts! (> (len name) u0) ERR-INVALID-STATION)
    (asserts! (> (len location) u0) ERR-INVALID-STATION)
    (map-set weather-stations new-id {
      name: name,
      location: location,
      owner: tx-sender,
      active: true,
      installed-at: stacks-block-height
    })
    ;; Set default alert thresholds
    (map-set alert-thresholds new-id {
      frost-temp: -200,        ;; -2C
      high-humidity: u8500,     ;; 85%
      low-soil-moisture: u2000, ;; 20%
      high-wind-speed: u5000    ;; 50 km/h
    })
    (var-set station-counter new-id)
    (ok new-id)
  )
)

;; Update alert thresholds for a station
(define-public (set-alert-thresholds 
  (station-id uint) 
  (frost-temp int) 
  (high-humidity uint) 
  (low-soil-moisture uint) 
  (high-wind-speed uint)
)
  (begin
    (asserts! (station-exists? station-id) ERR-INVALID-STATION)
    (asserts! (is-station-owner station-id tx-sender) ERR-UNAUTHORIZED)
    (asserts! (and (< frost-temp 500) (> frost-temp -1000)) ERR-INVALID-THRESHOLD)
    (asserts! (and (<= high-humidity u10000) (> high-humidity u0)) ERR-INVALID-THRESHOLD)
    (asserts! (and (<= low-soil-moisture u10000) (> low-soil-moisture u0)) ERR-INVALID-THRESHOLD)
    (asserts! (and (<= high-wind-speed u20000) (> high-wind-speed u0)) ERR-INVALID-THRESHOLD)
    (map-set alert-thresholds station-id {
      frost-temp: frost-temp,
      high-humidity: high-humidity,
      low-soil-moisture: low-soil-moisture,
      high-wind-speed: high-wind-speed
    })
    (ok true)
  )
)

;; Record weather reading and check for alerts
(define-public (record-weather-reading 
  (station-id uint)
  (temperature int)
  (humidity uint)
  (soil-moisture uint)
  (wind-speed uint)
  (precipitation uint)
)
  (let (
    (reading-id (+ (var-get station-counter) stacks-block-height))
    (current-time stacks-block-height)
  )
    (asserts! (station-exists? station-id) ERR-INVALID-STATION)
    (asserts! (is-station-owner station-id tx-sender) ERR-UNAUTHORIZED)
    (asserts! (and (> temperature -5000) (< temperature 6000)) ERR-INVALID-READING) ;; -50C to 60C
    (asserts! (<= humidity u10000) ERR-INVALID-READING) ;; 0-100%
    (asserts! (<= soil-moisture u10000) ERR-INVALID-READING) ;; 0-100%
    (asserts! (<= wind-speed u50000) ERR-INVALID-READING) ;; 0-500 km/h
    (asserts! (<= precipitation u100000) ERR-INVALID-READING) ;; 0-1000mm
    
    ;; Store the reading
    (map-set weather-readings reading-id {
      station-id: station-id,
      timestamp: current-time,
      temperature: temperature,
      humidity: humidity,
      soil-moisture: soil-moisture,
      wind-speed: wind-speed,
      precipitation: precipitation
    })
    
    ;; Check for frost conditions
    (if (check-frost-conditions station-id temperature)
      (map-set frost-alerts reading-id {
        station-id: station-id,
        alert-time: current-time,
        severity: (if (<= temperature -500) "HIGH" "MODERATE"),
        active: true
      })
      true
    )
    
    ;; Check for other alerts
    (match (map-get? alert-thresholds station-id)
      thresholds
      (begin
        ;; High humidity alert
        (if (>= humidity (get high-humidity thresholds))
          (map-set crop-alerts (+ reading-id u1) {
            station-id: station-id,
            alert-type: "HIGH_HUMIDITY",
            created-at: current-time,
            message: "High humidity detected - monitor for fungal diseases",
            active: true
          })
          true
        )
        ;; Low soil moisture alert
        (if (<= soil-moisture (get low-soil-moisture thresholds))
          (map-set crop-alerts (+ reading-id u2) {
            station-id: station-id,
            alert-type: "LOW_SOIL_MOISTURE",
            created-at: current-time,
            message: "Low soil moisture - consider irrigation",
            active: true
          })
          true
        )
        ;; High wind speed alert
        (if (>= wind-speed (get high-wind-speed thresholds))
          (map-set crop-alerts (+ reading-id u3) {
            station-id: station-id,
            alert-type: "HIGH_WIND",
            created-at: current-time,
            message: "High wind speeds - secure crops and equipment",
            active: true
          })
          true
        )
        true
      )
      true
    )
    
    (ok reading-id)
  )
)

;; Schedule irrigation
(define-public (schedule-irrigation 
  (station-id uint)
  (start-time uint)
  (duration uint)
  (frequency uint)
)
  (let (
    (schedule-id (+ station-id stacks-block-height))
  )
    (asserts! (station-exists? station-id) ERR-INVALID-STATION)
    (asserts! (is-station-owner station-id tx-sender) ERR-UNAUTHORIZED)
    (asserts! (> duration u0) ERR-INVALID-READING)
    (asserts! (> frequency u0) ERR-INVALID-READING)
    (asserts! (>= start-time stacks-block-height) ERR-INVALID-READING)
    
    (map-set irrigation-schedules schedule-id {
      station-id: station-id,
      start-time: start-time,
      duration: duration,
      frequency: frequency,
      active: true
    })
    (ok schedule-id)
  )
)

;; Deactivate station
(define-public (deactivate-station (station-id uint))
  (begin
    (asserts! (station-exists? station-id) ERR-INVALID-STATION)
    (asserts! (is-station-owner station-id tx-sender) ERR-UNAUTHORIZED)
    (match (map-get? weather-stations station-id)
      station (begin
        (map-set weather-stations station-id (merge station { active: false }))
        (ok true)
      )
      ERR-INVALID-STATION
    )
  )
)

;; Read-only Functions

;; Get station information
(define-read-only (get-station (station-id uint))
  (map-get? weather-stations station-id)
)

;; Get latest weather reading for a station
(define-read-only (get-latest-reading (reading-id uint))
  (map-get? weather-readings reading-id)
)

;; Get active frost alerts for a station
(define-read-only (get-frost-alert (alert-id uint))
  (map-get? frost-alerts alert-id)
)

;; Get irrigation schedule for a station
(define-read-only (get-irrigation-schedule (schedule-id uint))
  (map-get? irrigation-schedules schedule-id)
)

;; Get crop alerts for a station
(define-read-only (get-crop-alert (alert-id uint))
  (map-get? crop-alerts alert-id)
)

;; Get alert thresholds for a station
(define-read-only (get-alert-thresholds (station-id uint))
  (map-get? alert-thresholds station-id)
)

;; Get total number of stations
(define-read-only (get-station-count)
  (var-get station-counter)
)


