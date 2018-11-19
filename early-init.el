;;;; Early package configuration ;;;;

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)

;; Cache package setup code
(setq package-quickstart t)
