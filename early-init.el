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

;; Package configuration management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Always install packages if not already available
(setq use-package-always-ensure t)
