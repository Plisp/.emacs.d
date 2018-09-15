;;;; init.el --- My emacs init file with the majority of my configuration ;;;;

;;; Copyleft (c) 2018 Plisp ;;;
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

;;; Initial setup ;;;

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(require 'package)
;; Packages should not be activated
(unless package--initialized
  (package-initialize))

;; Package archives
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Package configuration management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; Always install if not already
(setq use-package-always-ensure t)

;;; end of: Initial setup ;;;

;;; Useful little snippets ;;;

;; Use utf-8
(setq locale-coding-system 'utf-8
      buffer-file-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Coding preference for pasted strings
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

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
  "Unmap"
  (interactive)
  (with-selected-frame frame            ; Note: this line is very important
    (progn
      (define-key input-decode-map [?\C-i] [C-i])
      (define-key input-decode-map [?\C-m] [C-m]))))

(add-to-list 'after-make-frame-functions 'unmap-keys t)

;; Stolen from emacs redux
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
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
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "<C-m>") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-m") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Don't use backspace
(global-set-key (kbd "C-S-d") 'backward-delete-char-untabify)

;; Better delete-word TODO
(defun clean-kill-word (word-at-point)
  "Delete separators"
  (if (string= (string (following-char))
               (string (preceding-char)))
      (delete-char 1)
    nil))

(advice-add 'kill-word :after 'clean-kill-word)

;; Need smartparens when using M-:
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

;; Familiar keybind for buffer switching
(defun switch-to-previous-buffer ()
  "Toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key [C-tab] 'switch-to-previous-buffer)
(global-set-key (kbd "C-j") 'newline-and-indent)

;; Char deletion
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;;; end of: Useful little snippets ;;;

;;; EmacsWiki packages ;;;

(require 'dired+)

;; Better window resizing https://github.com/ramnes/move-border
(require 'move-border)
(global-set-key (kbd "M-S-<up>") 'move-border-up)
(global-set-key (kbd "M-S-<down>") 'move-border-down)
(global-set-key (kbd "M-S-<left>") 'move-border-left)
(global-set-key (kbd "M-S-<right>") 'move-border-right)

;;; end of: EmacsWiki packages ;;;

;;; Important packages ;;;

;; Normally local variables
(setq-default ring-bell-function 'ignore
              version-control t
              fill-column 80)

(setq inhibit-startup-screen t
      delete-old-versions t
      auto-save-default nil
      backup-directory-alist `(("." . ,(edir "backups")))
      vc-make-backup-files t
      column-number-mode t)

;; Y-or-n is much faster
(fset 'yes-or-no-p 'y-or-n-p)

;; Simplify mode line symbols
(use-package diminish :demand t
  :config
  (diminish 'auto-revert-mode))

;; Very Large Files
(use-package vlf)

;; Help with finding cursor
(use-package beacon
  :diminish "_*_"
  :config (beacon-mode))

;; Color code highlighting
(use-package rainbow-mode)

;; Help with keybindings
(use-package which-key
  :diminish
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
  :init
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-height 10
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-count-format ""
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode))

;; Fuzzy ivy
                                        ;(use-package flx)

;; another fuzzy package
(use-package prescient
  :init
  (setq prescient-save-file (edir "presc-save.el")
        prescient-filter-method 'fuzzy)
  :config (prescient-persist-mode))

(use-package ivy-prescient
  :after (ivy prescient)
  :config (ivy-prescient-mode)
  (add-to-list 'ivy-prescient-excluded-commands 'counsel-rg))

;; Completion for commands
(use-package counsel
  :init
  (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number %s .")

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

  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c j" . counsel-git)        ; Find files
         ("C-C r" . counsel-rg)
         ("M-Y" . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . mu-counsel-search-project)))

;; Searching using ivy
(use-package swiper
  :init (setq case-fold-search t)
  :bind ("C-S-s" . swiper-all))

;; Project management
(use-package projectile
  :init
  (use-package projectile-ripgrep)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

;; Better projectile integration
(use-package counsel-projectile
  :init
  (setq counsel-projectile-grep-command "rg -i -M 120 --no-heading --line-number %s ."
        counsel-projectile-remove-current-buffer t)
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; Completion
(use-package company
  :init
  (add-to-list 'completion-styles 'initials t)
  (setq company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 3
        company-dabbrev-other-buffers 'all
        company-dabbrev-code-everywhere t)

  :bind (("C-M-/" . company-complete)
         :map company-active-map
         ;; Change 'input' to company candidates (not input-method)
         ("C-\\" . company-other-backend)
         ("C-o" . company-filter-candidates))
  :config (global-company-mode))

(use-package company-prescient
  :after (company prescient)
  :config (company-prescient-mode))

;; Fast jumping to places on window
(use-package avy
  :init (setq avy-timeout-seconds 0.2)
  :bind ("M-g M-g" . avy-goto-end-of-line))

(use-package ace-isearch
  :init
  (setq ace-isearch-function 'avy-goto-char
        ace-isearch-jump-delay 0.2
        ace-isearch-use-jump 'printing-char
        ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch
        ace-isearch-use-fallback-function t
        ace-isearch-fallback-function 'swiper)
  :config (global-ace-isearch-mode))

;; Fast window switching
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?k ?l))
  :bind ("M-o" . ace-window))

;; Fancy icons
(use-package all-the-icons
  :init
  (setq inhibit-compacting-font-caches t)
  ;; Dired support
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))
  ;; Ivy find file support
  (use-package all-the-icons-ivy
    :config (all-the-icons-ivy-setup)))

;; https://github.com/abo-abo/hydra

(use-package hydra    ;TODO
  :init)

;;; end of: Important packages ;;;

;;; Editing ;;;

(setq-default x-stretch-cursor t
              delete-selection-mode t
              indent-tabs-mode nil
              tab-width 2
              transient-mark-mode t
              scroll-preserve-screen-position t
              indicate-empty-lines t
              global-font-lock-mode t)

;; Faster commenting
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Hippie expand > dabbrev (no company backend unfortunately)
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Show highlighted region
(use-package volatile-highlights)

;; Advanced undo
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
         ("C-?" . undo-tree-redo))      ; Like C-/ for undo
  :config (global-undo-tree-mode))

;; Wrap lines at fill-column (as opposed to window edge)
(use-package visual-fill-column
  :hook ((visual-line-mode . visual-fill-column-mode)))

;; Show position in buffer
(use-package nyan-mode
  :hook (org-mode prog-mode)
  :config (nyan-toggle-wavy-trail))

(use-package markdown-mode :mode (".md" ".markdown"))
(use-package json-mode :mode (".json" ".imp"))

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
              ("M-}" . sp-forward-barf-sexp)
              ("C-("  . sp-backward-slurp-sexp)
              ("M-{"  . sp-backward-barf-sexp)

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
    (setq ispell-progsram-name "ispell"))

  :diminish "Flyspell"
  :bind ("<f7>" . flyspell-buffer)
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; honestly not as useful as I thought - there aren't ever that many corrections
(use-package flyspell-correct-ivy
  :after flyspell ivy
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

(global-set-key (kbd "C-c w") 'whitespace-mode)

;;; end of: Editing ;;;

;;; Programming ;;;

;; Don't let electric indent lines other than the current
(setq-default electric-indent-inhibit t)

(add-hook 'prog-mode-hook 'which-function-mode)

(require 'compile)

;; Local variables
(setq-default ediff-split-window-function 'split-window-horizontally)

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
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)))

(use-package sr-speedbar
  :init
  (setq speedbar-show-unknown-files t
        sr-speedbar-right-side nil))

;; Magit TODO setup
(use-package magit
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

;; Display git status
(use-package git-gutter
  :init (setq git-gutter:update-interval 5)
  :config (global-git-gutter-mode))

;; Snippets
(use-package yasnippet
  :init
  ;; Snippets
  (use-package yasnippet-snippets)
  ;(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :hook ((prog-mode . yas-minor-mode))
  :bind (("<C-i>" . yas-expand)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y h" . yas-describe-tables)
         ("C-c y r" . yas-reload-all)))

;; Linting
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-display-errors-function 'ignore)
  (use-package flycheck-tip))

;; Color based on error status

(use-package flycheck-color-mode-line
  :after flycheck
  :config (flycheck-color-mode-line-mode))

(use-package semantic
  :defer t
  :init
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (use-package stickyfunc-enhance)
  (require 'semantic/sb)
  :hook ((c-mode . semantic-mode)
         (c++-mode . semantic-mode))
  :bind (:map semantic-mode-map
              ("C-c s j" . semantic-ia-fast-jump)
              ("C-c s s" . semantic-ia-show-summary)
              ("C-c s d" . semantic-ia-show-doc)
              ("C-c s r" . semantic-symref))
  :config
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  (semantic-add-system-include "/usr/include/boost" 'c++-mode))

;; Assembly files
(use-package asm-mode
  :mode ("\\.s\\'"))

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

;; flash matching parentheses
                                        ;(add-hook 'after-init-hook 'show-paren-mode)

;; highlight lines
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Delete useless whitespace
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; end of: Programming ;;;

;;; Org mode ;;;

;; Possibly the most practical Emacs package I've ever used
;; Note: remember to ensure latest version (from org development repository) is installed for features
(use-package org
  :init
  (setq org-log-done 'time
        org-use-speed-commands t
        org-list-demote-modify-bullet t
        org-list-allow-alphabetical t
        org-src-tab-acts-natively t
        org-preview-latex-default-process 'convert
        org-default-notes-file "~/org/notes.org"
        org-archive-location "~/org/archive.org::* Archives"
        org-agenda-files '("~/org")
        org-agenda-restore-windows-after-quit t
        org-agenda-inhibit-startup nil
        org-agenda-category-icon-alist '(("School" '("📖"))
                                         ("System" '("💻"))
                                         ("Misc" '("🗡"))
                                         ("math" '("∮")))
        org-ellipsis " ⬎"
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("IN-PROGRESS" . (:inherit 'org-warning :foreground "blue"))
                                 ("CANCELLED" . (:inherit 'org-level-1 :foreground "yellow"))))
  :custom-face
  (org-done ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  (org-warning ((t (:family "DejaVu Sans Mono" :height 135 :bold t))))
  (variable-pitch ((t (:family "DejaVu Sans" :height 130))))
  :hook ((org-mode . (lambda () (progn (visual-line-mode) (org-indent-mode))))
         (text-scale-mode . visual-fill-column-adjust))
  :mode ("\\.txt\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)))

;; fancy bullets
(use-package org-bullets
  :after org
  :init (setq org-bullets-bullet-list '("◉" "◇" "○" "⚫"))
  :hook (org-mode . org-bullets-mode))

(use-package zpresent
  :init
  (setq zpresent-bullet "→"
        zpresent-delete-other-windows t
        zpresent-default-background-color "#b7b063"
        zpresent-fullscreen-on-zpresentation t)

  :hook (zpresent-mode . (lambda () (setq fill-column 1000))))

;; For those moments when I can't think of clever sounding things to write
(use-package academic-phrases)

;; Web browsing TODO setup
(use-package w3m
  :defer t)

;;; Programming languages ;;;

;; (require 'setup-c)
;; (require 'setup-cl);

;;; Aesthetics ;;;

;; Fallback font for utf-8 chars
(set-fontset-font "fontset-default" nil (font-spec :size 90 :name "Symbola"))

;; Default font
(set-face-attribute 'default nil :height 95)
(set-face-attribute 'mode-line nil :height 95 :family "DejaVu Sans Mono")

(setq default-frame-alist '((font . "Hack")
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (horizontal-scroll-bar . nil)
                            (vertical-scroll-bars . nil)
                            (background-mode . 'dark)
                            (fullscreen . maximized)
                            (left-fringe . 2)
                            (right-fringe . 5)))

;; Remove obnoxious line between windows
(set-face-foreground 'vertical-border "#073642" nil)

(use-package solarized-theme
  :init
  (setq solarized-high-contrast-mode-line t
        solarized-use-variable-pitch t
        solarized-scale-org-headlines nil
        solarized-distinct-fringe-background t)

  (defun solarized-dark-theme ()
    "Activate solarized-dark for lisp"
    (interactive)
    (load-theme 'solarized-dark t)
    (set-face-attribute 'mode-line nil :underline nil :overline nil :box nil)))

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  ;; Fix funky mode line problems with color theme and powerline
  (set-face-attribute 'mode-line nil :underline nil :overline nil :box nil)
  (setq spacemacs-theme-keyword-italic t
        spacemacs-theme-org-height t
        spacemacs-theme-underline-parens t))

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
                                         major-mode :face face1 :height 0.9 :v-adjust 0.01) face1 'l)
                                       (powerline-major-mode face1 'l)

                                       (when (and (boundp 'which-function-mode) which-function-mode)
                                         (powerline-raw which-func-format face1 'l))

                                       (powerline-process face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-raw "[" face2 'l)

                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2))

                                       (powerline-raw "]" face2)))

                            (rhs (list (powerline-raw " ;) " face2)
                                       (powerline-raw
                                        (all-the-icons-fileicon
                                         "emacs":face face2 :height 0.9 :v-adjust 0.01) face2 'r)
                                       (funcall separator-right face2 face1)
                                       (powerline-raw " " face1)
                                       (powerline-minor-modes face1 'l)
                                       (powerline-raw "  " face1)
                                       (funcall separator-right face1 face0)

                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face0 'l))

                                       (powerline-raw "%o" face0 'l)
                                       (powerline-raw "ln %l" face0 'l)
                                       (powerline-raw " : " face0)
                                       (powerline-raw "col %c" face0)

                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))

                                       (powerline-fill face0 0))))

                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
  :custom-face
  (powerline-active1 ((t (:background "#2d353a" :foreground "#ccc"))))
  (powerline-active2 ((t (:background "#5c656b" :foreground "#eee"))))
  :config (my-powerline-theme))

;;;; init.el ends here
