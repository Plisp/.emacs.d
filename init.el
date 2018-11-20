;;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:
;; My Emacs init file th the majority of my configuration

;;; Copyleft (c) 2018 Plisp
;;
;; Terms and Conditions
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.
;;
;;; If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Ignore old bytecode
(setq load-prefer-newer t)

;; Package archives
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

;; package-quickstart does not run package-initialize
(advice-add 'package-install :before '(lambda (package) (package-initialize)))

;;; Package configuration management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Always install packages if not already available
(setq use-package-always-ensure t)

;; No error checking necessary
(setq use-package-expand-minimally t)

;;; Benchmarking
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Misc. setup

;; Used several times
(eval-and-compile
  (define-inline user-dir (path)
    (expand-file-name path user-emacs-directory)))

;; Stray lisp files
(add-to-list 'load-path (user-dir "elisp"))

;; load custom file w/o drama
(setq custom-file (user-dir "custom.el"))

(load custom-file t t)

;; Need to use server-after-make-frame-hook
(require 'server)

;; Unmap TAB, RET, ESC from C-i, C-m, C-[ (note: this only works in GUI)
(defun unmap-keys ()
  "Unmap C-m from RET, C-i from TAB, C-[ from ESC.
So that I can use the keys elsewhere"
  (interactive)
  (if (display-graphic-p)
      (when (daemonp)
        (progn
          (define-key input-decode-map [?\C-i] [C-i])
          (define-key input-decode-map [?\C-m] [C-m])
          (define-key input-decode-map [?\C-\[] [C-\[])))))

(add-to-list 'server-after-make-frame-hook 'unmap-keys t)

;; Stolen from emacs redux
(defun smarter-move-beginning-of-line (arg)
  "Effectively toggle between the first non-whitespace character
and the beginning of the line depending on arg."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(defun push-mark-no-activate ()
  "Pushes point to mark ring and does not activate the region.
Equivalent to `set-mark-command' when `transient-mark-mode' is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; remap M-m (`back-to-indentation' functionality is provided by C-a) to `push-mark-no-activate'
(global-set-key (kbd "M-m") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the mark ring order.
`  This is the same as using `set-mark-command' with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; remap C-m to `jump-to-mark'
(global-set-key (kbd "<C-m>") 'jump-to-mark)

;; Don't use backspace
(global-set-key (kbd "C-S-d") 'backward-delete-char-untabify)

;; Remap C-j to replace RET
(global-set-key (kbd "C-j") 'newline-and-indent)

;; Char deletion
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; ;; C-z is my personal prefix TODO write bindings
;; (global-unset-key (kbd "C-z"))

;; (defalias 'z-keymap (make-sparse-keymap))

;; (global-set-key "\C-z" 'z-keymap)

;; Cause killing the server to ask first
(add-hook 'kill-emacs-hook 'save-some-buffers)

(global-set-key [remap save-buffers-kill-terminal] 'kill-emacs)

;;; end of: Misc. setup
;;; Essential packages

;; Hippie expand > dabbrev (no company backend unfortunately)
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill))

;; Simplify mode line symbols
(use-package diminish :demand t
  :config
  (diminish 'auto-revert-mode)
	(diminish 'eldoc-mode))

;; Install binaries using system package manager
(use-package use-package-ensure-system-package :demand t)

;; Help with finding cursor
(use-package beacon
  :diminish "_*_"
  :config (beacon-mode))

;; Help with keybindings
(use-package which-key
  :diminish
  :init (setq which-key-idle-delay 0.5)
  :config (which-key-mode))

;; https://github.com/abo-abo/hydra TODO configure and write hydras
(use-package hydra
  :defer t
  :init
  (setq hydra-verbose t))

;; Minibuffer completion frontend
(use-package ivy
	:diminish
  :init (ivy-mode)
  (setq enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-descbinds . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-ignore-buffers '("\\.org"))
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-c C-r" . ivy-reverse-i-search)
         ("C-r" . ivy-previous-line-or-history)))

;; premade hydras for ivy
(use-package ivy-hydra
  :after (ivy hydra))

;; ;; another fuzzy package (not quite good enough yet)
;; (use-package prescient
;;   :init (setq prescient-save-file (user-dir "presc-save.el"))
;;   :config (prescient-persist-mode))

;; (use-package ivy-prescient
;;   :after (ivy prescient)
;;   :config (ivy-prescient-mode)
;;   (add-to-list 'ivy-prescient-excluded-commands 'counsel-rg)
;;   (add-to-list 'ivy-prescient-filter-method-keys '("C-c C-f" (literal+initialism . fuzzy))))

;; Minibuffer completion all the things!
(use-package counsel
  :ensure-system-package ((rg . ripgrep) (ag . the_silver_searcher))
  :init (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."
              counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
              counsel-git-cmd "rg --files")
  :bind (("C-x C-f" . counsel-find-file)
         ("C-c j" . counsel-git)
         ("C-c r" . counsel-rg)
         ("M-Y" . counsel-yank-pop)
         ("<f5>" . counsel-unicode-char)
         ("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap apropos-command] . counsel-apropos)))

;; Better fuzzy matching for smex
(use-package flx)

;; flx integration for smex
(use-package flx-ido
  :after flx
  :config (flx-ido-mode))

;; Counsel-M-x unnecessarily obscures my window
(use-package smex
  :after flx
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Searching with regex and context
(use-package swiper
  :after (ivy counsel)
  :init
  (setq case-fold-search t
        swiper-action-recenter t
        swiper-goto-start-of-match t)
  :bind (("C-S-s" . swiper-all)))

;;; end of: Essential packages
;;; General settings

(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              ring-bell-function 'ignore
              tab-width 2
              ;; files.el
              version-control t
              ;; ediff-wind.el
              ediff-split-window-function 'split-window-horizontally)

(setq scroll-preserve-screen-position t
      transient-mark-mode t
      x-stretch-cursor t
      ;; Files.el
      delete-old-versions t
      save-silently t
      auto-save-default nil
      backup-directory-alist `(("." . ,(user-dir "backups")))
      ;; vc-hooks.el
      vc-make-backup-files t
      vc-follow-symlinks t
      ;; simple.el
      column-number-mode t
      ;; del-sel.el
      delete-selection-mode t
      ;; startup.el
      initial-scratch-message "Emacs is love, Emacs is life"
      ;; font-core.el
      global-font-lock-mode t)

(defvar *my-hyperspec-location* "file://home/plisp/.roswell/lisp/quicklisp/dists/quicklisp/software/clhs-0.6.3/HyperSpec-7-0/HyperSpec/")

;; Y-or-n is much faster
(fset 'yes-or-no-p 'y-or-n-p)

;; Use utf-8 everywhere
(setq locale-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Coding preference for pasted strings
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Fallback font for utf-8 chars
(set-fontset-font "fontset-default" nil (font-spec :size 90 :name "Symbola"))

;; Default font
(set-face-attribute 'default nil :height 124)

;;; end of: General settings
;;; EmacsWiki packages

;; Better dired (note: it is highly recommended to byte compile this)
(use-package dired+ :ensure nil
  :disabled t
  :after dired
  :init (setq dired-listing-switches "-alh")
  :hook (dired-mode . (lambda () (dired-hide-details-mode -1))))

;; Better window resizing: https://github.com/ramnes/move-border
(use-package move-border :ensure nil
  :bind (("M-S-<up>" . move-border-up)
         ("M-S-<down>" . move-border-down)
         ("M-S-<left>" . move-border-left)
         ("M-S-<right>" . move-border-right)))

;; Get weather status in mode line: https://github.com/zk-phi/sky-color-clock
(use-package sky-color-clock :ensure nil
  :disabled t
  :config
  ;; Change to your region's latitude
  (sky-color-clock-initialize -34)
  ;; Sign up on openweathermap for API key and city IDs
  (sky-color-clock-initialize-openweathermap-client "ce527c830c7fee7d3b0efc7e4c84da58" 6619279))

;; Flip between buffers fast: https://github.com/jrosdahl/iflipb
(use-package iflipb :ensure nil
  :bind (([M-tab] . iflipb-next-buffer)
         ([M-iso-lefttab] . iflipb-previous-buffer))) ; Meta-Shift-Tab

;; i3 integration: https://github.com/vava/i3-emacs
(use-package i3 :ensure nil
  :init (require 'i3-integration)
  (i3-one-window-per-frame-mode-on))

;;; end of: EmacsWiki packages
;;; Editing

;; Very Large Files
(use-package vlf :no-require)

;; Color code highlighting
(use-package rainbow-mode
	:diminish
  :hook (prog-mode . rainbow-mode))

;; Edit grep results directly
(use-package wgrep :no-require)

;; Smart mark
(use-package smart-region
  :bind ("C-SPC" . smart-region))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-+" . mc/mark-all-like-this)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)))

;; Fast jumping to places on window
(use-package avy
  :init (setq avy-timeout-seconds 0.2)
  :bind (("M-g M-g" . avy-goto-end-of-line)
         ("M-c" . avy-goto-char)))

;; Fast window switching
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?j ?k ?l))
  :bind ("M-o" . ace-window))

;; Support for some important filetypes
(use-package markdown-mode :mode (".md" ".markdown"))

(use-package json-mode :mode (".json" ".imp"))

(use-package asm-mode :ensure nil
  :hook (asm-mode . (lambda () (setq-local tab-stop-list (number-sequence 2 60 2)))))

;; Show highlighted region
(use-package volatile-highlights
	:diminish
  :config (volatile-highlights-mode))

;; Advanced undo
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
         ("C-?" . undo-tree-redo))
  :config (global-undo-tree-mode))

;; Show position in buffer
(use-package nyan-mode
  :hook (org-mode prog-mode)
  :config (nyan-toggle-wavy-trail))

;; Parentheses management
(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (c-mode . smartparens-mode)
         (c++-mode . smartparens-mode)
         (sly-mrepl-mode  . smartparens-mode)
         (eval-expression-minibuffer-setup . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-("  . sp-backward-slurp-sexp)
              ("C-{"  . sp-backward-barf-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-<backspace>" . backward-kill-word)
              ("M-<backspace>" . sp-backward-kill-word)
              ([remap comment-line] . sp-comment)
              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)
              ("C-x C-t" . sp-transpose-hybrid-sexp)
              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c _"  . wrap-with-underscores)
              ("C-c `"  . wrap-with-back-quotes))
  :config
  (show-smartparens-global-mode)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(c-mode c++-mode) "'" "'" :actions nil)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))))

;; Completion
(use-package company
	:diminish
  :init (global-company-mode)
  (setq company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 3
        company-dabbrev-other-buffers 'all
        completion-styles '(initials basic partial-completion))
  :bind (("C-M-/" . company-complete)
         :map company-active-map
         ;; YCM style
         ([tab] . company-complete-common-or-cycle)
         ;; Change 'input' to company candidates (as opposed to input-method)
         ("C-\\" . company-other-backend)
         ("C-d" . company-show-doc-buffer)
         ("C-o" . company-filter-candidates)
         ;; Jump to completion source
         ("M-." . company-show-location)))

;; Popup tips for company
(use-package company-quickhelp
  :after company
  :init (setq company-quickhelp-delay 0.2)
  :config (company-quickhelp-mode))

;; (use-package company-prescient
;;   :after (company prescient)
;;   :config (company-prescient-mode))

;; Spell checking
(use-package flyspell-correct
  :ensure-system-package aspell
  :diminish "Flyspell"
  :init
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq flyspell-issue-welcome-flag nil)
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  :bind ("<f7>" . flyspell-buffer))

;; Correct mistakes with ivy
(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;;; end of: Editing
;;; Programming

;; ;; Don't let electric indent lines other than the current
;; (setq-default electric-indent-inhibit t)

(use-package compile :ensure nil
  :defer t
  :init
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error))

;; Project management
(use-package projectile
  :init (projectile-mode)

  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Ripgrep integration for projectile
(use-package projectile-ripgrep
  :ensure-system-package (rg . ripgrep)
  :after projectile)

;; Better projectile integration
(use-package counsel-projectile
  :after (counsel projectile)
  :init
  (setq counsel-projectile-remove-current-buffer t)
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; Rainbow parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Symbol highlighting+editing
(use-package symbol-overlay
  :diminish "symO"
  :bind (("M-i" . symbol-overlay-put)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-n" . symbol-overlay-jump-next))
  :config (symbol-overlay-mode))

;; Code folding
(use-package origami
  :init
  (setq origami-show-fold-header t)
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-c o t" . origami-recursively-toggle-node)
              ("C-c o T" . origami-toggle-all-nodes)
              ("C-c o o" . origami-show-only-node)
              ("C-c o /" . origami-undo)
              ("C-c o ?" . origami-redo))
  :config (global-origami-mode))

;; Magit TODO setup
(use-package magit
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

;; Display git status in fringe
(use-package git-gutter
	:diminish
  :init (setq git-gutter:update-interval 5)
  :config (global-git-gutter-mode))

;; Snippets
(use-package yasnippet
  :hook (c-mode-common . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y h" . yas-describe-tables)
         ("C-c y r" . yas-reload-all))
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

;; Snippets for yasnippet
(use-package yasnippet-snippets
  :after yasnippet)

;; Linting
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode 'right-fringe)
  :hook (c-mode-common . flycheck-mode))

;; Show tips for errors
(use-package flycheck-tip
  :after flycheck
  ;; Show nothing in echo area
  :init (setq flycheck-display-errors-function 'ignore)
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-tip-cycle)
              ("C-c ! p" . flycheck-tip-cycle-reverse)))

;; (defun semantic-remove-hooks ()
;;   "Fix buggy completion when semantic is enabled: https://github.com/syl20bnr/spacemacs/issues/11058"
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-completion-at-point-function)
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-notc-completion-at-point-function)
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-nolongprefix-completion-at-point-function))

;; ;; Semantic code parser (too slow for large projects, but incremental update is good for small projects)
;; (use-package semantic
;;   :ensure stickyfunc-enhance
;;   :defer t
;;   :init
;;   (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;   :hook ((c-mode-common . semantic-mode)
;;          (semantic-mode . semantic-remove-hooks))
;;   :bind (:map semantic-mode-map
;;               ("C-c s j" . semantic-ia-fast-jump)
;;               ("C-c s s" . semantic-ia-show-summary)
;;               ("C-c s d" . semantic-ia-show-doc)
;;               ("C-c s r" . semantic-symref))
;;   :config
;;   (semanticdb-enable-gnu-global-databases 'c-mode t)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode t)
;;   (semantic-add-system-include "/usr/include/boost" 'c++-mode))

;; Keep font lock for certain keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (when (not (derived-mode-p 'cmake-mode)) ; Highlighting in cmake-mode interferes with font lock
              (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face t))))))

;; Highlight current line
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Display function in mode line
(add-hook 'prog-mode-hook 'which-function-mode)

;; Line truncation is terrible to read
(add-hook 'server-after-make-frame-hook 'toggle-truncate-lines)

;; Delete useless whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically make scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; end of: Programming
;;; Org mode and miscellaneous

;; Note: remember to ensure latest version (from org development repository) is installed for features
(use-package org
  :after all-the-icons
  :init
  (setq org-log-done 'time
        org-use-speed-commands t
        org-list-demote-modify-bullet t
        org-list-allow-alphabetical t
        org-src-tab-acts-natively t
        org-preview-latex-default-process 'convert
        org-archive-location "~/org/archive.org::* Archives"
        org-agenda-files '("~/org/misc.org" "~/org/school.org" "~/org/system.org")
        org-agenda-restore-windows-after-quit t
        org-agenda-inhibit-startup nil
        org-ellipsis " ⬎"
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))
        org-agenda-category-icon-alist `(("misc" ,(list (all-the-icons-faicon "bomb")) nil nil :ascent center)
                                         ("system" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
                                         ("school" ,(list (all-the-icons-faicon "book")) nil nil :ascent center))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("IN-PROGRESS" . (:inherit org-warning :foreground "blue"))
                                 ("CANCELLED" . (:inherit org-level-1 :foreground "yellow"))))
  :custom-face
  (org-done ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  (org-warning ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (text-mode . orgstruct-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)))

;; fancy bullets
(use-package org-bullets
  :after org
  :init (setq org-bullets-bullet-list '("◉" "◇" "○" "⚫"))
  :hook (org-mode . org-bullets-mode))

;; presentations - note darkroom package
(use-package zpresent
  :disabled t
  :init
  (setq zpresent-bullet "→"
        zpresent-delete-other-windows t
        zpresent-default-background-color "#073642"
        zpresent-fullscreen-on-zpresentation t))

;; For those moments when I can't think of clever sounding things to write
(use-package academic-phrases
  :after org)

;; Automatic capitalisation
(use-package captain                    ; TODO set this up properly
  :diminish
  :hook ((text-mode . (lambda () (setq captain-predicate (lambda () t))))
         (org-mode . (lambda () (setq captain-predicate (lambda () t)))))
  :config (global-captain-mode))

;; Web browsing within emacs
(use-package w3m
  :init
  (setq w3m-use-cookies t
        browse-url-browser-function 'w3m-browse-url)
  :hook (w3m-fontify-after . visual-line-mode)
  :bind (("C-c w u" . w3m-goto-url)
         ("C-c w ." . browse-url-at-point)
         ("C-c w s" . w3m-search)
         :map w3m-mode-map
         ("&" . w3m-view-url-with-external-browser)))

;; URL hinting with avy
(use-package link-hint
  :bind (("C-c h o" . link-hint-open-link)
         ("C-c h y" . link-hint-copy-link)))

;;; end of: Org mode and miscellaneous
;;; Aesthetics

(setq default-frame-alist '((font . "DejaVu Sans Mono")
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (horizontal-scroll-bar . nil)
                            (vertical-scroll-bars . nil)
                            (fullscreen . maximized)
                            (left-fringe . 2)
                            (right-fringe . 6)))

;; Fancy icons
(use-package all-the-icons
  :init
  ;; Don't cause garbage collection during startup
  (setq inhibit-compacting-font-caches t))

;; Dired support
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Ivy support
(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :config (all-the-icons-ivy-setup))

;; Attach to hooks
(defun server-theme-setup (theme)
  (if (not (display-graphic-p))
      (progn
        (remove-hook 'prog-mode-hook 'hl-line-mode)
        (set-face-attribute 'company-tooltip nil :background "#120"))
    (load-theme theme t)
    (set-face-attribute 'mode-line nil :height 120 :family "Hack")))

(use-package solarized-theme
  :disabled t
  :no-require
  :init
  (setq solarized-scale-org-headlines t)
  :custom-face
  (mode-line ((t (:underline nil))))
  (mode-line-inactive ((t (:underline nil))))
  :hook (server-after-make-frame . (lambda () (server-theme-setup 'solarized-light))))

(use-package gruvbox-theme
  ;;:disabled t
  :no-require
  :custom-face
  (company-tooltip-common-selection ((t (:foreground "#d787af"))))
  :hook (server-after-make-frame . (lambda () (server-theme-setup 'gruvbox-dark-soft))))

;; Mode line eye candy
(use-package powerline
  :init
  (setq powerline-default-separator 'arrow)
  ;; My powerline - now with fancy inline symbols (note: remember to change icon size when font size changes)
  (defun my-powerline-theme ()
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))

                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))

                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))

                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (when (buffer-file-name (current-buffer))
                                         (powerline-buffer-id `(mode-line-buffer-id ,face0) 'r))
                                       (powerline-vc face0 'r)
                                       (when (and (buffer-file-name (current-buffer)) vc-mode)
                                         (if (and window-system (not powerline-gui-use-vcs-glyph))
                                             (powerline-raw
                                              (all-the-icons-octicon
                                               "git-branch"
                                               :face face0 :v-adjust 0.05) face0 'r)))

                                       (funcall separator-left face0 face1)

                                       (when (assoc major-mode all-the-icons-mode-icon-alist)
                                         (powerline-raw
                                          (all-the-icons-icon-for-mode
                                           major-mode
                                           :face face1 :height 0.9 :v-adjust 0.01) face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (when (and (not (equal major-mode 'org-mode))
                                                  (boundp 'which-function-mode)
                                                  which-function-mode)
                                         (powerline-raw which-func-format face1 'l))
                                       (powerline-process face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)

                                       (funcall separator-left face1 face2)

                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face2 'l))
                                       (powerline-raw "[" face2 'l)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2))
                                       (powerline-raw "]" face2)
                                       (powerline-raw "%l:%c " face2 'l)))

                            (rhs (list (funcall separator-right face2 face1)
                                       (powerline-raw " " face1)
                                       (powerline-minor-modes face1 'r)

                                       (funcall separator-right face1 face0)

                                        ;(powerline-raw (sky-color-clock) face0 'l)
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))
                                       (powerline-fill face0 0))))

                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
  :custom-face
  ;; (powerline-active0 ((t (:background "#191229" :foreground "#888"))))
  ;; (powerline-active1 ((t (:background "#2d353a" :foreground "#bbb"))))
  ;; (powerline-active2 ((t (:background "#5c656b" :foreground "#eee"))))
  :config (my-powerline-theme))

;;; end of: Aesthetics
;;; Programming languages

;;; Common lisp setup
(use-package sly
  :load-path "~/.emacs.d/elpa/sly-20181116.2131/"
  :no-require
  :init
  (setq sly-lisp-implementations '((roswell ("ros" "-Q" "run") :coding-system utf-8-unix))
        common-lisp-hyperspec-root *my-hyperspec-location*))

;;; C languages

(add-hook 'c-mode-common-hook (lambda ()
                                (progn (setq-local indent-tabs-mode t)
                                       (setq-local tab-width 4)
                                       (setq-local c-basic-offset 4)
                                       (setq-local fill-column 80)
                                       (toggle-truncate-lines))))

;; Cmake files
(use-package cmake-mode
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda () (add-to-list 'company-backends 'company-cmake))))

(use-package cmake-font-lock
  :after cmake-mode
  :config (cmake-font-lock-activate))

;; Clang format takes care of style control
(use-package clang-format
  :bind ("C-c f" . clang-format-region))

;; Better c++14 highlighting
(use-package modern-cpp-font-lock
  :hook (c++-mode .  modern-c++-font-lock-global-mode))

;; Don't color include directives
(add-hook 'c-mode-common-hook (lambda () (rainbow-mode 0)))

;; IDE
(require 'setup-c)

;; Restore sensible GC default
(setq gc-cons-threshold 20000000)

;;; init.el ends here
