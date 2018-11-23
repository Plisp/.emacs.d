;;;; early-init.el --- Early init file loaded before package-initialize

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)

;; Automatically refresh the quickstart file after package installation
(setq package-quickstart t)
