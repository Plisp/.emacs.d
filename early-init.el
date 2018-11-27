;;;; early-init.el --- Early init file loaded before package-initialize

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000
      gc-cons-percentage 0.6)

;; The file handler incurs overhead
(defvar original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Automatically refresh the quickstart file after package installation
(setq package-quickstart t)
