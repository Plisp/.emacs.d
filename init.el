;;;; init.el --- My emacs init file with the majority of my configuration ;;;;

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

;;; Initial setup

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)

;; Package management TODO switch to straight.el
(require 'package)

;; Packages should not be activated
(unless package--initialized
  (package-initialize))

;; Package archives
(setq package-archives '(("melpa"     . "http://melpa.org/packages/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Package configuration management
(unless (package-installed-p 'use-package)
  ;(package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Always install packages if not already available
(setq use-package-always-ensure t)

;;; end of: Initial setup
;;; Useful little snippets

;; Use utf-8
(setq locale-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Coding preference for pasted strings
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Used several times
(eval-and-compile
  (define-inline edir (path)
    (expand-file-name path user-emacs-directory)))

;; Stray lisp files
(add-to-list 'load-path (edir "elisp"))

;; load custom file w/o drama
(setq custom-file (edir "custom.el"))
(load custom-file t t)

;; Shut down server instance - use window manager for closing frames
(defun kill-server ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Unmap TAB and RET from C-i, C-m (legacy bindings)
(defun unmap-keys (frame)
  "Unmap C-m from RET and C-i from TAB"
  (interactive)
  ;; Never try to remap in tty - I only use emacsclient with GUI anyways
  (when (daemonp)
    (with-selected-frame frame                  ; Note: this line is very important
      (progn
        (define-key input-decode-map [?\C-i] [C-i])
        (define-key input-decode-map [?\C-m] [C-m])))))

(add-to-list 'after-make-frame-functions 'unmap-keys t)

;; Stolen from emacs redux
(defun smarter-move-beginning-of-line (arg)
  "Effectively toggle between the first non-whitespace character and
the beginning of the line."
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
(define-key global-map [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(defun push-mark-no-activate ()
  "Pushes point to mark-ring and does not activate the region
   Equivalent to `set-mark-command' when `transient-mark-mode' is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; remap C-m to `push-mark-no-activate'
(global-set-key (kbd "<C-m>") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the mark-ring order.
`  This is the same as using `set-mark-command' with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; remap M-m (`back-to-indentation' functionality is provided by C-a) to `jump-to-mark'
(global-set-key (kbd "M-m") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to `exchange-point-and-mark' but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; remap `exchange-point-and-mark' to `exchange-point-and-mark-no-activate'
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Don't use backspace
(global-set-key (kbd "C-S-d") 'backward-delete-char-untabify)

;; Need smartparens when using minibuffer evaluation
(add-hook 'eval-expression-minibuffer-setup-hook 'smartparens-mode)

;; C-m is easier to remember as a mark command
(global-set-key (kbd "C-j") 'newline-and-indent)

;; Char deletion
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; C-z is my personal prefix TODO write bindings
(global-unset-key (kbd "C-z"))
(defalias 'z-keymap (make-sparse-keymap))
(define-key global-map "\C-z" 'z-keymap)

;;; end of: Useful little snippets
;;; External packages

;; Better dired
(require 'dired+)

;; Better window resizing: https://github.com/ramnes/move-border
(require 'move-border)
(global-set-key (kbd "M-S-<up>") 'move-border-up)
(global-set-key (kbd "M-S-<down>") 'move-border-down)
(global-set-key (kbd "M-S-<left>") 'move-border-left)
(global-set-key (kbd "M-S-<right>") 'move-border-right)

;; ;; Get weather status in mode line: https://github.com/zk-phi/sky-color-clock
;; (require 'sky-color-clock)
;; ;; Change to your region's latitude
;; (sky-color-clock-initialize -34)
;; ;; Sign up to get API key and download country codes
;; (sky-color-clock-initialize-openweathermap-client "ce527c830c7fee7d3b0efc7e4c84da58" 6619279)

;; Flip between buffers fast: https://github.com/jrosdahl/iflipb
(require 'iflipb)
(global-set-key [M-tab] 'iflipb-next-buffer)
(global-set-key [M-iso-lefttab] 'iflipb-previous-buffer)

;; i3 integration: https://github.com/vava/i3-emacs
(require 'i3)
(require 'i3-integration)
(i3-one-window-per-frame-mode-on)

;;; end of: External packages
;;; Important packages and settings

(setq-default ring-bell-function 'ignore
              version-control t)

(setq delete-old-versions t
      save-silently t
      auto-save-default nil
      backup-directory-alist `(("." . ,(edir "backups")))
      vc-make-backup-files t
      column-number-mode t)

;; Y-or-n is much faster
(fset 'yes-or-no-p 'y-or-n-p)

;; ;; Wrap words at window edge
;; (global-visual-line-mode)

;; Simplify mode line symbols
(use-package diminish :demand t
  :config
  (diminish 'auto-revert-mode)
	(diminish 'eldoc-mode))

;; Very Large Files
(use-package vlf
  :init (require 'vlf-setup))

;; Help with finding cursor
(use-package beacon
  :diminish "_*_"
  :config (beacon-mode))

;; Color code highlighting
(use-package rainbow-mode
	:diminish
  :hook (prog-mode . rainbow-mode))

;; Help with keybindings
(use-package which-key
  :diminish
  :init (setq which-key-idle-delay 0.5)
  :config (which-key-mode))

;; Edit grep results directly
(use-package wgrep)

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

;; Regex matching for everything
(use-package ivy
	:diminish
  :init (ivy-mode)
  (use-package ivy-hydra)
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-height 12
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

;; ;; another fuzzy package (not quite good enough yet)
;; (use-package prescient
;;   :init (setq prescient-save-file (edir "presc-save.el"))
;;   :config (prescient-persist-mode))

;; (use-package ivy-prescient
;;   :after (ivy prescient)
;;   :config (ivy-prescient-mode)
;;   (add-to-list 'ivy-prescient-excluded-commands 'counsel-rg)
;;   (add-to-list 'ivy-prescient-filter-method-keys '("C-c C-f" (literal+initialism . fuzzy))))

;; Minibuffer completion (note I don't want to replace all the functions)
(use-package counsel
  :init (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number %s .")
  :bind (("C-x C-f" . counsel-find-file)
         ("C-c j" . counsel-git)
         ("C-c u" . counsel-unicode-char)
         ("C-c r" . mu-counsel-search-project)
         ("M-Y" . counsel-yank-pop)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap apropos-command] . counsel-apropos)))

(defun mu-counsel-search-project (initial-input &optional use-current-dir)
  "Search using `counsel-rg' from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
  (interactive (list (thing-at-point 'symbol)
                     current-prefix-arg))
  (let ((current-prefix-arg)
        (dir (if use-current-dir
                 default-directory
               (condition-case err
                   (projectile-project-root)
                 (error default-directory)))))
    (funcall 'counsel-rg initial-input dir)))

;; Better fuzzy matching for smex
(use-package flx
  :config
  (use-package flx-ido)
  (flx-ido-mode))

;; Counsel-M-x unnecessarily obscures my window
(use-package smex
  :after flx
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Searching with regex and context
(use-package swiper
  :after ivy
  :init (setq case-fold-search t)
  ;; These keys are C-Shift-s and M-Shift-s respectively
  :bind (("C-S-s" . swiper)
         ("M-S" . swiper-all)))

;; Fuzzy isearching
(use-package flx-isearch
  :after flx
  :bind (("C-s" . flx-isearch-forward)
         ("C-r" . flx-isearch-backward)))

;; Project management
(use-package projectile
  :init (projectile-mode)
  (use-package projectile-ripgrep)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Better projectile integration
(use-package counsel-projectile
  :init
  (setq counsel-projectile-grep-command "rg -i -M 120 --no-heading --line-number %s ."
        counsel-projectile-remove-current-buffer t)
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; Completion
(use-package company
	:diminish
  :init
  (setq company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 3
        company-dabbrev-other-buffers 'all
        completion-styles '(initials basic partial-completion))

  :bind (("C-M-/" . company-complete)
         :map company-active-map
         ;; Change 'input' to company candidates (as opposed to input-method)
         ("C-\\" . company-other-backend)
         ("C-d" . company-show-doc-buffer)
         ("C-o" . company-filter-candidates)
         ("M-." . company-show-location))
  :config (global-company-mode))

;; Popup tips for company
(use-package company-quickhelp
  :init (setq company-quickhelp-delay 0.5)
  :config (company-quickhelp-mode))

;; (use-package company-prescient
;;   :after (company prescient)
;;   :config (company-prescient-mode))

;; Fast jumping to places on window
(use-package avy
  :init (setq avy-timeout-seconds 0.2)
  :bind (("M-g M-g" . avy-goto-end-of-line)
         ("M-c" . avy-goto-char)))

;; Fast window switching
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?j ?k ?l))
  :bind ("M-o" . ace-window))

;; https://github.com/abo-abo/hydra TODO configure and write hydras
(use-package hydra
  :init
  (setq hydra-verbose t))

;; Quick shell
(use-package shell-pop
  :init
  (setq shell-pop-term-shell "/bin/bash"
        shell-pop-full-span t)
  :bind ("M-`" . shell-pop))

;;; end of: Important packages
;;; Editing

(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines t)

(setq x-stretch-cursor t
      delete-selection-mode t
      transient-mark-mode t
      scroll-preserve-screen-position t
      global-font-lock-mode t
      auto-revert-verbose nil)

;; Faster commenting
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Hippie expand > dabbrev (no company backend unfortunately)
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill))

;; Support for some important filetypes
(use-package asm-mode :mode ("\\.s\\'"))
(use-package markdown-mode :mode (".md" ".markdown"))
(use-package json-mode :mode (".json" ".imp"))

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
  :hook (prog-mode . smartparens-strict-mode)
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

  :config (show-smartparens-global-mode)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(c-mode c++-mode) "'" "'" :actions nil)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))))

;; Spell checking
(use-package flyspell
  :init
  (setq flyspell-issue-welcome-flag nil)
  (if (executable-find "aspell")
      (progn
        (setq ispell-program-name "aspell")
        (setq ispell-extra-args '("--sug-mode=ultra")))
    (message "Install aspell and dictionary"))

  :diminish "Flyspell"
  :bind ("<f7>" . flyspell-buffer)
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; Correct mistakes with ivy
(use-package flyspell-correct-ivy
  :after flyspell ivy
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

;;; end of: Editing
;;; Programming

(setq-default ediff-split-window-function 'split-window-horizontally
              ;; Don't let electric indent lines other than the current
              electric-indent-inhibit t)

(require 'compile)

(setq vc-follow-symlinks t
      compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

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

;; Display git status
(use-package git-gutter
	:diminish
  :init (setq git-gutter:update-interval 5)
  :config (global-git-gutter-mode))

;; Snippets
(use-package yasnippet
  :init
  ;; Snippets
  (use-package yasnippet-snippets)
  (yas-global-mode)
  ;; Use this to expand snippets while in company-active-map
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y h" . yas-describe-tables)
         ("C-c y r" . yas-reload-all)))

;; Linting
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode 'right-fringe)
  :hook (prog-mode . flycheck-mode))

;; Show tips for errors
(use-package flycheck-tip
  :after flycheck
  ;; Show nothing in echo area
  :init (setq flycheck-display-errors-function 'ignore)
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-tip-cycle)
              ("C-c ! p" . flycheck-tip-cycle-reverse)))

;; ;; Color the mode line based on error status
;; (use-package flycheck-color-mode-line
;;   :after flycheck
;;   :config (flycheck-color-mode-line-mode))

(use-package semantic
  :defer t
  :init
  (use-package stickyfunc-enhance)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

  :hook ((c-mode-common . semantic-mode)
         (semantic-mode . semantic-remove-hooks))

  :bind (:map semantic-mode-map
              ("C-c s j" . semantic-ia-fast-jump)
              ("C-c s s" . semantic-ia-show-summary)
              ("C-c s d" . semantic-ia-show-doc)
              ("C-c s r" . semantic-symref))

  :config
  (require 'semantic/sb)
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  (semantic-add-system-include "/usr/include/boost" 'c++-mode))

;; Fix buggy completion when semantic is enabled: https://github.com/syl20bnr/spacemacs/issues/11058
(defun semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions 'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions 'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions 'semantic-analyze-nolongprefix-completion-at-point-function))

;; Cmake files
(use-package cmake-mode
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda () (add-to-list 'company-backends 'company-cmake))))

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . (lambda () (cmake-font-lock-activate))))

;; Better c++14 highlighting
(use-package modern-cpp-font-lock
  :hook (c++-mode .  modern-c++-font-lock-global-mode))

;; Keep font lock for certain keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (when (not (derived-mode-p 'cmake-mode)) ; Highlighting in cmake-mode interferes with font lock
              (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face t))))))

;; highlight lines
(add-hook 'prog-mode-hook 'hl-line-mode)
;; Display function in mode line
(add-hook 'prog-mode-hook 'which-function-mode)
;; Delete useless whitespace
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; Automatically make scripts executable
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; end of: Programming
;;; Org mode and miscellaneous

;; Note: remember to ensure latest version (from org development repository) is installed for features
(use-package org
  :init
  (setq org-log-done 'time
        org-use-speed-commands t
        org-list-demote-modify-bullet t
        org-list-allow-alphabetical t
        org-src-tab-acts-natively t
        org-preview-latex-default-process 'convert
        ;org-default-notes-file "~/org/notes.org"
        org-archive-location "~/org/archive.org::* Archives"
        org-agenda-files '("~/org" "~/school")
        org-agenda-restore-windows-after-quit t
        org-agenda-inhibit-startup nil
        org-ellipsis " ⬎"
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))

        org-agenda-category-icon-alist `(("misc" ,(edir "bin/dagger_knife.svg") nil nil :ascent center)
                      ("system" ,(edir "bin/personal_computer.svg") nil nil :ascent center)
                      ("math" ,(edir "bin/contour_integral.svg") nil nil :ascent center)
                      ("school" ,(edir "bin/open_book.svg") nil nil :ascent center))

        org-todo-keyword-faces '(("TODO" . org-warning)
                      ("IN-PROGRESS" . (:inherit org-warning :foreground "blue"))
                      ("CANCELLED" . (:inherit org-level-1 :foreground "yellow"))))

  :custom-face
  (org-done ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  (org-warning ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  (variable-pitch ((t (:family "DejaVu Sans" :height 130))))

  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (text-mode . orgstruct-mode)
         (org-after-todo-state-change . org-archive-done))

  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))

  :config
  )


;; Archive completed items automatically
(defun org-archive-done ()
  (when (org-entry-is-done-p)
    (org-archive-subtree-default)))

;; fancy bullets
(use-package org-bullets
  :after org
  :init (setq org-bullets-bullet-list '("◉" "◇" "○" "⚫"))
  :hook (org-mode . org-bullets-mode))

;; presentations - note darkroom package
(use-package zpresent
  :init
  (setq zpresent-bullet "→"
        zpresent-delete-other-windows t
        zpresent-default-background-color "#b7b063"
        zpresent-fullscreen-on-zpresentation t))

;; For those moments when I can't think of clever sounding things to write
(use-package academic-phrases)

(use-package captain                    ; TODO set this up properly
  :hook ((text-mode . (lambda () (setq captain-predicate (lambda () t))))
         (org-mode . (lambda () (setq captain-predicate (lambda () t)))))
  :config (global-captain-mode))

;; Web browsing
(use-package w3m
  :init
  (setq browse-url-browser-function 'w3m-browse-url
        w3m-use-cookies t
        w3m-default-display-inline-images t)

  :hook (w3m-fontify-after . visual-line-mode)
  :bind (("C-c w u" . w3m-goto-url)
         ("C-c w ." . browse-url-at-point)
         ("C-c w s" . w3m-search)
         :map w3m-mode-map
         ("&" . w3m-view-url-with-external-browser)))

;; URL hinting
(use-package link-hint
  :bind (("C-c h o" . link-hint-open-link)
         ("C-c h y" . link-hint-copy-link)))

;;; end of: Org mode and miscellaneous
;;; Aesthetics

;; Fallback font for utf-8 chars
(set-fontset-font "fontset-default" nil (font-spec :size 90 :name "Symbola"))

;; Default font
(set-face-attribute 'default nil :height 109)

(setq default-frame-alist '((font . "Hack")
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (horizontal-scroll-bar . nil)
                            (vertical-scroll-bars . nil)
                            (background-mode . dark)
                            (fullscreen . maximized)
                            (left-fringe . 2)
                            (right-fringe . 6)))

;; Remove obnoxious line between windows
(set-face-foreground 'vertical-border "#073642" nil)

;; Fancy icons
(use-package all-the-icons
  :defer 2
  :init
  ;; Dired support
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))
  ;; Ivy find file support
  (use-package all-the-icons-ivy
    :config (all-the-icons-ivy-setup))
  ;; Don't cause garbage collection during startup
 (setq inhibit-compacting-font-caches t))

(use-package spacemacs-theme
  :disabled t
  :init
  (load-theme 'spacemacs-dark t)
  ;; Fix funky mode line problems with color theme and powerline
  (set-face-attribute 'mode-line nil :height 100 :family "DejaVu Sans Mono" :underline nil :overline nil :box nil)
  (setq spacemacs-theme-keyword-italic t
        spacemacs-theme-org-height t
        spacemacs-theme-underline-parens t))

(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t)
  ;; Fix funky mode line problems with color theme and powerline
  (set-face-attribute 'mode-line nil :height 100 :family "DejaVu Sans Mono" :underline nil :overline nil :box nil))

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
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face0 'l))
                                       (powerline-vc face0 'r)
                                       (when (and (buffer-file-name (current-buffer)) vc-mode)
                                         (if (and window-system (not powerline-gui-use-vcs-glyph))
                                             (powerline-raw
                                              (all-the-icons-octicon "git-branch" :face face0 :v-adjust 0.05)
                                              face0 'r)))

                                       (funcall separator-left face0 face1)

                                       (powerline-raw
                                        (all-the-icons-icon-for-mode
                                         major-mode
                                         :face face1 :height 0.9 :v-adjust 0.01) face1 'l)
                                       (powerline-major-mode face1 'l)
                                       (when (and (not (equal major-mode 'org-mode))
                                                  (boundp 'which-function-mode)
                                                  which-function-mode)
                                         (powerline-raw which-func-format face1 'l))
                                       (powerline-process face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)

                                       (funcall separator-left face1 face2)

                                       (powerline-raw "[" face2 'l)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2))
                                       (powerline-raw "] " face2)
                                       (powerline-raw "%o" face2 'r)))

                            (rhs (list (funcall separator-right face2 face1)
                                       (powerline-raw " " face1)
                                       (powerline-minor-modes face1 'r)

                                       (funcall separator-right face1 face0)

                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face0 'l))
                                       (powerline-raw "%l:%c " face0 'l)
                                       ;(powerline-raw (sky-color-clock))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))
                                       (powerline-fill face0 0))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))

  :custom-face
  (powerline-active0 ((t (:background "#191229" :foreground "#888"))))
  (powerline-active1 ((t (:background "#2d353a" :foreground "#bbb"))))
  (powerline-active2 ((t (:background "#5c656b" :foreground "#eee"))))
  :config (my-powerline-theme))

;;; end of: Aesthetics
;;; Programming languages

(setq gc-cons-threshold 20000000)

(require 'setup-c)
;; (require 'setup-cl)

;;;; init.el ends here ;;;;
