;;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file contains the majority of my configuration

;; Copyleft (c) 2018 Plisp
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
;; If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Ignore old bytecode
(setq load-prefer-newer t)

;; Package archives
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;;("org"   . "https://orgmode.org/elpa/")
                         ))

;;; Package configuration management
(require 'package)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

;; use-package is not needed at runtime
(eval-when-compile
  (require 'use-package))

(require 'diminish)

;; dependency of use-package
(require 'bind-key)

;; searching
;; (use-package selectrum
;;   :init (selectrum-mode))
;; (use-package selectrum-prescient
;;   :init
;;   (selectrum-prescient-mode)
;;   (prescient-persist-mode))

;; (use-package ctrlf
;;   :init (ctrlf-mode))

;; Always install packages if not already available
(setq use-package-always-ensure t)

;; Always defer unless explicitly specified otherwise
;;(setq use-package-always-defer t)

;; No error checking necessary
(setq use-package-expand-minimally t)

;;; Benchmarking
(use-package esup)

;;; Misc. setup

;; Used several times
(eval-when-compile
  (define-inline user-dir (path)
    (expand-file-name path user-emacs-directory)))

;; Stray lisp files
(add-to-list 'load-path (user-dir "elisp"))

;; load custom file w/o drama
(setq custom-file (user-dir "custom.el"))
(load custom-file t t)

;; Needed to use server-after-make-frame-hook
(require 'server)

(defun unmap-keys ()
  "Unmap C-m from RET, C-i from TAB, C-[ from ESC.
So that they can be used elsewhere"
  (when (display-graphic-p)
    (progn
      (define-key input-decode-map [?\C-i] [C-i])
      (define-key input-decode-map [?\C-m] [C-m])
      (define-key input-decode-map [?\C-\[] [C-\[]))))

;; Will run if not using daemon
(unmap-keys)

;; If using daemon, run after client display loads
(add-hook 'server-after-make-frame-hook 'unmap-keys)

;; Stolen from emacs redux
(defun smarter-move-beginning-of-line (arg)
  "Effectively toggle between the first non-whitespace character
and the beginning of the line depending on arg."
  (interactive "^p")
  (setq arg (or arg 1))

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

;; Delete word before point
(global-set-key (kbd "M-D") 'backward-kill-word)

;; Char deletion
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; ;; C-z is my personal prefix TODO write bindings
;; (global-unset-key (kbd "C-z"))

;; (defalias 'z-keymap (make-sparse-keymap))

;; (global-set-key "\C-z" 'z-keymap)

;; Cause killing the server to ask first
(add-hook 'kill-emacs-hook #'save-some-buffers)

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
;; breaks because package names vary - causes 100% cpu bug
;; (use-package use-package-ensure-system-package)

;; Help with finding cursor
(use-package beacon
  :diminish "_*_"
  :config (beacon-mode))

;; Help with keybindings
(use-package which-key
  :defer 1
  :diminish
  :config (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; https://github.com/abo-abo/hydra TODO configure and write hydras
(use-package hydra
  :config
  (setq hydra-verbose t))

;; Searching with regex and context
(use-package swiper
  :after (ivy counsel)
  :bind ("C-S-s" . swiper-all)
  :config
  (setq case-fold-search t
        swiper-action-recenter t
        swiper-goto-start-of-match t))

;; Minibuffer completion frontend
(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)  ; Usually the first command I run
         :map ivy-minibuffer-map
         ("C-c C-r" . ivy-reverse-i-search)
         ("C-r" . ivy-previous-line-or-history))
  :hook (minibuffer-setup . ivy-mode)
  :config
  (setq enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-descbinds . ivy--regex-plus)
                                (counsel-rg . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(defun maybe-bind-C-r (file)
  (unless (equal mode-name "mrepl")
    (local-set-key (kbd "C-r") 'counsel-grep-or-swiper)))

;;Minibuffer completion all the things!
(use-package counsel
  ;;:ensure-system-package ((rg . ripgrep) (ag . the_silver_searcher))
  :bind (("C-x C-f" . counsel-find-file)
         ("C-c j" . counsel-git)
         ("C-c r" . counsel-rg)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("M-Y" . counsel-yank-pop)
         ("<f5>" . counsel-unicode-char)
         ("C-s" . counsel-grep-or-swiper)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap apropos-command] . counsel-apropos))
  :init
  ;; little hack to prevent overriding of sly's excellent reverse isearch
  ;;(add-hook 'after-load-functions #'maybe-bind-C-r)
  (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."
        counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s"
        counsel-git-cmd "rg --files"))

;; Counsel-M-x unnecessarily obscures my window
(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (use-package flx :demand t)
  (use-package flx-ido :demand t
    :config (flx-ido-mode)))

;; flx integration for smex
(use-package flx-ido
  :config (flx-ido-mode))

;;; end of: Essential packages
;;; General settings

(setq-default fill-column 89
              fringes-outside-margins t
              hscroll-margin 1
              indent-tabs-mode nil
              indicate-empty-lines t
              ring-bell-function 'ignore
              tab-width 2
              ;; ediff-wind.el
              ediff-split-window-function 'split-window-horizontally
              ;; files.el
              version-control t)

(setq auto-window-vscroll nil
      inhibit-startup-screen t
      scroll-conservatively 1001
      scroll-preserve-screen-position t
      transient-mark-mode t
      use-dialog-box nil
      x-stretch-cursor t
      word-wrap t
      ;; advice.el
      ad-redefinition-action 'accept
      ;; del-sel.el
      delete-selection-mode t
      ;; files.el
      delete-old-versions t
      save-silently t
      auto-save-default nil
      backup-directory-alist `(("." . ,(user-dir "backups")))
      ;; font-core.el
      global-font-lock-mode t
      ;; novice.el
      disabled-command-function nil
      ;; select.el
      select-enable-clipboard t
      select-enable-primary t
      ;; simple.el
      blink-matching-delay -1
      column-number-mode t
      ;; startup.el
      initial-scratch-message (shell-command-to-string "fortune | cowsay")
      ;; vc-hooks.el
      vc-make-backup-files t
      vc-follow-symlinks t)

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
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Default font
(set-face-attribute 'default nil :height 117 :font "Input Mono")

;; Color theme
(defvar *color-theme* 'solarized-dark-high-contrast)

;; Local hyperspec location
(defvar *my-hyperspec-location* "file:/home/plisp/quicklisp/dists/quicklisp/software/clhs-0.6.3/HyperSpec-7-0/HyperSpec/")

;; Better window resizing: https://github.com/ramnes/move-border
(use-package move-border
  :ensure nil
  :bind (("M-S-<up>" . move-border-up)
         ("M-S-<down>" . move-border-down)
         ("M-S-<left>" . move-border-left)
         ("M-S-<right>" . move-border-right)))

;; (use-package edwina
;;   :ensure t
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys)
;;   (edwina-mode 1))

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
  ;;:disabled t
  :bind (([M-tab] . iflipb-next-buffer)
         ([M-iso-lefttab] . iflipb-previous-buffer)) ; Meta-Shift-Tab
  :config (setq iflipb-wrap-around t))

(autoload #'dired-jump-other-window "/usr/local/share/emacs/27.0.50/lisp/dired-x.elc")
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)

;;; end of: EmacsWiki packages
;;; Editing

;; Very Large Files
(use-package vlf)

;; Color code highlighting
(use-package rainbow-mode)

;; Edit grep results directly
(use-package wgrep)

;; Smart mark
(use-package smart-region
  :bind ("C-SPC" . smart-region))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this-symbol)
         ("C->" . mc/mark-next-like-this-symbol)
         ("C-+" . mc/mark-all-like-this)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)))

;; Fast jumping to places on window
(use-package avy
  :bind (("M-g M-g" . avy-goto-line)
         ("M-c" . avy-goto-char))       ; Tip: Use M-c RET to goto end of line
  :config (setq avy-timeout-seconds 0.2))

(global-set-key (kbd "M-o") 'other-window)

;; Support for some important filetypes
(use-package markdown-mode :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package json-mode :mode ("\\.json\\'" "\\.imp\\'"))

(use-package asm-mode :ensure nil
  :hook (asm-mode . (lambda () (setq-local tab-stop-list (number-sequence 2 60 2)))))

;; Show highlighted region
(use-package volatile-highlights
  :diminish
  :config (volatile-highlights-mode))

;; Advanced undo
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
         ("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo))
  :config (global-undo-tree-mode))

;; Parentheses management
(use-package smartparens
  :bind (:map smartparens-mode-map
              ;; Movement
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)
              ;; Paredit style slurping and barfing
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ;; Misc operations
              ("M-]" . sp-unwrap-sexp)
              ("M-[" . sp-backward-unwrap-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("C-S-k" . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-t" . sp-transpose-hybrid-sexp)
              ("C-<backspace>" . sp-change-enclosing)
              ("M-<backspace>" . sp-change-inner)
              ([remap comment-line] . sp-comment))
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (c-mode . smartparens-mode)
         (c++-mode . smartparens-mode)
         (sly-mrepl-mode  . smartparens-mode)
         (eval-expression-minibuffer-setup . smartparens-mode))
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0)
  (show-smartparens-global-mode)
  ;; Language specific configuration
  (sp-local-pair '(emacs-lisp-mode lisp-mode sly-mrepl-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(c-mode c++-mode) "'" "'" :actions nil))

;; Completion
(use-package company
  :defer 2
  :diminish
  :bind (("C-M-/" . company-complete)
         :map company-active-map
         ("C-\\" . company-other-backend) ; Change 'input' to company candidates (as opposed to input-method)
         ("M-." . company-show-location) ; Jump to completion source
         ("C-o" . company-filter-candidates))
  :config (global-company-mode)
  (setq company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-other-buffers 'all
        completion-styles '(initials basic partial-completion)
        company-require-match nil))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Popup tips for company
;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode)
;;   :config (company-quickhelp-mode)
;;   (setq company-quickhelp-delay 0.1))

;; Spell checking
(use-package flyspell-correct
  ;;:ensure-system-package aspell
  :diminish "Flyspell"
  :bind ("<f7>" . flyspell-buffer)
  :hook ((org-mode text-mode) . flyspell-mode)
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-issue-welcome-flag nil))

;; Correct mistakes with ivy
;; (use-package flyspell-correct-ivy
;;   :after (flyspell ivy)
;;   :bind (:map flyspell-mode-map
;;               ("C-;" . flyspell-correct-wrapper)))

;;; end of: Editing
;;; Programming

(use-package nlinum
  :config
  (setq nlinum-format "%d "
        nlinum-highlight-current-line t))

(use-package nlinum-relative
  ;;:hook (prog-mode . nlinum-relative-on)
  :config
  (setq nlinum-relative-redisplay-delay 0))

;; (use-package ivy-xref
;;   :after ivy
;;   :init
;;   (setq xref-show-definitions-function #'ivy-xref-show-defs))

(use-package compile :ensure nil
  :config
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error))

;; Project management
(use-package projectile
  :defer 2
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq ;projectile-completion-system 'ivy TODO
        projectile-enable-caching t
        projectile-tags-command "etags -R -f \"%s\" %s \"%s\""))

;; Ripgrep integration for projectile
(use-package projectile-ripgrep
  ;;:ensure-system-package (rg . ripgrep)
  :after projectile)

;; Better projectile integration
(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode)
  (setq counsel-projectile-remove-current-buffer t))

;; Rainbow parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Symbol highlighting+editing
(use-package symbol-overlay
  :diminish "symO"
  :bind (("M-s" . symbol-overlay-put)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-n" . symbol-overlay-jump-next)))

;; Code folding
(use-package origami
  :bind (:map origami-mode-map
              ("C-c o t" . origami-recursively-toggle-node)
              ("C-c o T" . origami-toggle-all-nodes)
              ("C-c o o" . origami-show-only-node)
              ("C-c o /" . origami-undo)
              ("C-c o ?" . origami-redo))
  :config (global-origami-mode)
  (setq origami-show-fold-header t))

;; Magit
(use-package magit
  :bind ("C-c g" . magit-status))

;; Display git status in fringe
(use-package git-gutter
  :diminish
  :config (global-git-gutter-mode)
  (setq git-gutter:update-interval 5))

;; Snippets - only for argument completion purposes
(use-package yasnippet
  :hook (c-mode-common . yas-minor-mode))

;; Linting
(use-package flycheck
  :hook (c-mode-common . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode 'right-fringe))

;; Show tips for errors
(use-package flycheck-tip
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-tip-cycle)
              ("C-c ! p" . flycheck-tip-cycle-reverse)) ;
  :init (setq flycheck-display-errors-function 'ignore)) ; Show nothing in echo area

;; (defun semantic-remove-hooks ()
;;   "Fix buggy completion when semantic is enabled: https://github.com/syl20bnr/spacemacs/issues/11058"
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-completion-at-point-function)
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-notc-completion-at-point-function)
;;   (remove-hook 'completion-at-point-functions 'semantic-analyze-nolongprefix-completion-at-point-function))

;; ;; Semantic code parser (too slow for large projects, but incremental update is good for small projects)
;; (use-package semantic
;;   :ensure stickyfunc-enhance
;;   :defer t
;;   :bind (:map semantic-mode-map
;;               ("C-c s j" . semantic-ia-fast-jump)
;;               ("C-c s s" . semantic-ia-show-summary)
;;               ("C-c s d" . semantic-ia-show-doc)
;;               ("C-c s r" . semantic-symref))
;;   :hook ((c-mode-common . semantic-mode)
;;          (semantic-mode . semantic-remove-hooks))
;;   :config
;;   (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;   (semanticdb-enable-gnu-global-databases 'c-mode t)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode t)
;;   (semantic-add-system-include "/usr/include/boost" 'c++-mode))

;; Keep font lock for certain keywords everywhere
(add-hook 'prog-mode-hook
          (lambda ()
            (when (not (derived-mode-p 'cmake-mode)) ; Highlighting in cmake-mode interferes with font lock
              (font-lock-add-keywords nil '(("\\<\\(XXX\\|TODO\\)" 1 font-lock-warning-face t))))))

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Abbreviate certain keywords as symbols
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

;; Highlight matching parens
(add-hook 'prog-mode-hook #'show-paren-mode)
(setq show-paren-delay 0)

;; Display function in mode line
(add-hook 'prog-mode-hook #'which-function-mode)

;; Delete useless whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Automatically make scripts executable
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; end of: Programming
;;; Org mode and miscellaneous

;; Note: remember to ensure latest version (from org development repository) is installed for features
(use-package org :ensure org-plus-contrib ; :pin org
  :after all-the-icons
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
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
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))
        org-agenda-category-icon-alist `(("misc" ,(list (all-the-icons-faicon "bomb")) nil nil :ascent center)
                                         ("system" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
                                         ("school" ,(list (all-the-icons-faicon "book")) nil nil :ascent center))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("IN-PROGRESS" . (:inherit org-warning :foreground "green"))
                                 ("CANCELLED" . (:inherit org-done :foreground "yellow")))))

;; fancy bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("◉" "◇" "○" "⚫")))

;; presentations - note darkroom package
(use-package zpresent
  :disabled t
  :config
  (setq zpresent-bullet ""
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
  :init (setq browse-url-browser-function 'w3m-browse-url)
  :bind (("C-c w u" . w3m-goto-url)
         ("C-c w ." . browse-url-at-point)
         ("C-c w s" . w3m-search)
         :map w3m-mode-map
         ("&" . w3m-view-url-with-external-browser))
  :hook (w3m-fontify-after . visual-line-mode)
  :config
  (setq w3m-use-cookies t))

;; URL hinting with avy
(use-package link-hint
  :bind (("C-c h o" . link-hint-open-link)
         ("C-c h y" . link-hint-copy-link)))

(use-package auctex
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

;;; end of: Org mode and miscellaneous
;;; Aesthetics

;; Remove junk
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq left-fringe-width 2
      right-fringe-width 6)

;; Fancy icons
(use-package all-the-icons
  :defer 0.4
  :init
  ;; Don't cause garbage collection during startup
  (setq inhibit-compacting-font-caches t))

;; Dired support
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Ivy support
;; (use-package all-the-icons-ivy
;;   :after (all-the-icons ivy)
;;   :init (all-the-icons-ivy-setup))

;; Lots of color themes
(use-package doom-themes
  :config
  (setq doom-Iosvkem-comment-bg t)
  ;; these currently have no effect
  ;;(setq doom-molokai-comment-bg t
  ;;      doom-molokai-brighter-modeline t)
  (doom-themes-org-config))

;; Solarized
(use-package solarized-theme
  :init
  (setq solarized-distinct-doc-face t
        solarized-emphasize-indicators t
        solarized-scale-org-headlines t))

;; Add a temporary hook if running daemon
(defun pl-color-theme-setup ()
  (progn
    (load-theme *color-theme* t)))

;; Will run if not using daemon
(when (display-graphic-p) (pl-color-theme-setup))

;; If using daemon, run after client display loads
(when (daemonp)
  (add-hook 'server-after-make-frame-hook (lambda ()
                                            (progn
                                              (pl-color-theme-setup)
                                              (remove-hook 'server-after-make-frame-hook 'pl-color-theme-setup)))))

;; Mode line eye candy
(use-package powerline
  :defer 1
  ;;:disabled t
  :after all-the-icons
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


                                       (powerline-raw "%l:%c " face2 'l)
                                       (powerline-raw "[" face2 'l)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2))
                                       (powerline-raw "]" face2)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face2 'l)))
)
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

;; Show position in buffer
(use-package nyan-mode
  :init (nyan-mode)
  :after powerline
  :config
  ;;(nyan-start-music)
  ;;(nyan-start-animation)
  (nyan-toggle-wavy-trail))

;;; end of: Aesthetics
;;; Programming languages

;;; Common lisp setup

(defvar *my-fasldir* "fasl/")

(defun my-sly-compile-file ()
  (interactive)
  (let* ((rootdir (projectile-project-root))
         (fasldir (concat (projectile-project-root) *my-fasldir*))
         (relative-dir (string-trim-right
                        (substring (buffer-file-name (current-buffer)) (length rootdir))
                        "[^/]+"))
         (file-fasl-dir (concat fasldir relative-dir)))
    (make-directory file-fasl-dir t)
    (setq sly-compile-file-options (list :fasl-directory file-fasl-dir))
    (sly-compile-file)))

(use-package sly
  :bind (:map sly-editing-mode-map ("C-c C-k" . #'my-sly-compile-file))
  :config
  (setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "1000") :coding-system utf-8-unix)
                                   (ecl ("ecl") :coding-system utf-8-unix)
                                   (ccl ("ccl") :coding-system utf-8-unix)

                                   (roswell ("ros" "-Q" "run") :coding-system utf-8-unix))
        common-lisp-hyperspec-root *my-hyperspec-location*))

;;; C languages

(add-hook 'c-mode-common-hook #'(lambda () (setq indent-tabs-mode t)))

(use-package lsp-mode
  :hook ((c-mode . (lambda () (add-to-list 'company-backends 'company-capf) (require 'lsp-clangd) (lsp)))
         (c++-mode . (lambda () (add-to-list 'company-backends 'company-capf) (require 'lsp-clangd) (lsp))))
  :config
  (setq lsp-prefer-flymake nil))


;; (use-package ccls
;;   :hook ((c-mode . (lambda () (add-to-list 'company-backends 'company-lsp) (require 'lsp-clangd) (lsp)))
;;          (c++-mode . (lambda () (add-to-list 'company-backends 'company-lsp) (require 'lsp-clangd) (lsp))))
;;   :config
;;   (setq ccls-sem-highlight-method 'font-lock))

(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-lsp-enable-recompletion t
        company-lsp-async t
        company-lsp-enable-snippet t))

(use-package lsp-ui
  :after cc-mode
  :bind (("C-M-." . lsp-ui-peek-find-definitions)
         ("C-M-?" . lsp-ui-peek-find-references)))

;; Cmake support
(use-package cmake-mode
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda () (add-to-list 'company-backends 'company-cmake))))

(use-package cmake-font-lock
  :after cmake-mode
  :init (cmake-font-lock-activate))

;; Restore sensible GC default
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 20000000
                                          gc-cons-percentage 0.1)))

;; Restore file-name-handler-alist to original value
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist original-file-name-handler-alist)))

;;; init.el ends here
