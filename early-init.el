;;;; Early package configuration ;;;;

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)

(require 'package)

;; Create cache for faster startup
(setq package-quickstart t)

;; Package archives
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
