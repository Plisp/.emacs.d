;;;; initfile --- My emacs init file ;;;;
;;; Commentary:
;;; A work in progress
;;; Copyleft (c) 2018 Plisp ;;;

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;;; Initial setup ;;;

;; Garbage collection should happen later
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(require 'package)
;; Packages should not be activated
(unless package--initialized
  (package-initialize))

;; Package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Package configuration management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always install if not already
(setq use-package-always-ensure t)

;; Auto compile init files TODO fix warnings
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (setq load-prefer-newer t))

;;; Utilities ;;;

(eval-and-compile
  (define-inline edir (path)
    (expand-file-name path user-emacs-directory)))

;;; Useful snippets ;;;

(add-to-list 'load-path (edir "elisp"))

;; Move generated custom-file
(setq custom-file (edir "custom.el"))
(load custom-file t t)

(defun kill-server ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

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

(global-set-key (kbd "M-m") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;;; Important packages and helpful settings ;;;

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
(use-package diminish :ensure t :demand t)

;; Very Large Files
(use-package vlf :ensure t)

;; Help with finding cursor
(use-package beacon :ensure t
  :config
  (beacon-mode))

;; Help with keybindings
(use-package which-key
  :diminish
  :config
  (which-key-mode))

;; Project management
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (use-package projectile-ripgrep))

;; Edit grep results directly
(use-package wgrep)

;; Smart mark
(use-package smart-region
  :bind ("C-SPC" . smart-region))

;; Multiple cursors TODO learn to use
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines))

;; Regex matching for everything
(use-package ivy
  :diminish
  :config
  (ivy-mode)
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-height 10
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil
        ivy-count-format ""))

;; Fuzzy ivy
(use-package flx)

;; Completion for commands
(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git-grep)   ; Fast grep
         ("C-c j" . counsel-git)        ; Find files
         ("C-r" . counsel-rg)
         ("M-Y" . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add))
  :config
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number '%s' %s"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number %s ."))

;; Searching using ivy
(use-package swiper
  :bind
  ("C-s" . swiper)
  :config
  (setq case-fold-search t))

;; Fast jumping to places on window
(use-package avy
  :bind (("M-s" . avy-goto-char)        ; I didn't often use M-s
         ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(add-hook 'fancy-startup-screen 'emacs-startup-hook)

;; Familiar keybind for fast buffer back and forth
(defun switch-to-previous-buffer ()
  "Toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<C-tab>") 'switch-to-previous-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)

;;; Editing ;;;
(setq-default x-stretch-cursor t
              delete-selection-mode t
              tab-width 2
              transient-mark-mode t
              scroll-preserve-screen-position t
              indicate-empty-lines t
              global-font-lock-mode t
              tab-always-indent 'complete)

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
(use-package undo-tree)   ;TODO set bindings

;; Wrap lines at fill-column (as opposed to window edge)
(use-package visual-fill-column
  :hook ((visual-line-mode . visual-fill-column-mode)
         (text-scale-mode . visual-fill-column-adjust)))

;; Show position in buffer
(use-package nyan-mode
  :hook (org-mode prog-mode)
  :config
  (nyan-toggle-wavy-trail))

(use-package nlinum
  :hook (slime-repl-mode-hook . (lambda () (nlinum-mode 0)))
  :config
  (global-nlinum-mode)
  (setq nlinum-highlight-current-line t
        nlinum-format (concat "%"
                              (number-to-string (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                              "d\u2502")))

(use-package markdown-mode :mode (".md" ".markdown"))
(use-package json-mode :mode (".json" ".imp"))

;; Parentheses management
(use-package smartparens
  :hook (prog-mode . smartparens-strict-mode)
  :bind
  (:map smartparens-mode-map
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
        ("C-M-d" . delete-sexp)

        ("M-<backspace>" . backward-kill-word)
        ("C-<backspace>" . sp-backward-kill-word)
        ([remap sp-backward-kill-word] . backward-kill-word)

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
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))))

;; Spell checking
(use-package flyspell
  :diminish "flyspell"
  :bind ("<f7>" . flyspell-buffer)
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq flyspell-issue-welcome-flag nil)
  (if (executable-find "aspell")
      (progn
        (setq ispell-program-name "aspell")
        (setq ispell-extra-args '("--sug-mode=ultra")))
    (setq ispell-progsram-name "ispell")))

;; Use ivy matching
(use-package flyspell-correct-ivy
  :after flyspell ivy)

;;; Programming ;;;
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
  :diminish "sym"
  :bind (("M-i" . symbol-overlay-put)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-n" . symbol-overlay-jump-next)))

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

;; Magit TODO learn
(use-package magit
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :config)

;; Display git status
(use-package git-gutter)

;; Completion
(use-package company
  :init
  (add-to-list 'completion-styles 'initials t)
  (setq company-idle-delay 0
        company-tooltip-align-annotations t
        company-minimum-prefix-length 3
        company-dabbrev-other-buffers 'all)
  :bind (("C-M-/" . company-complete)
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("M-/" . company-other-backend)
               ("C-o" . company-filter-candidates)))
  :config
  (global-company-mode))

;; Snippets
(use-package yasnippet
  :diminish "yasnippet"
  :hook (prog-mode . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y h" . yas-describe-tables)
         ("C-c y r" . yas-reload-all)
         (:map yas-minor-mode-map
               ("C-i" . yas-next-field-or-maybe-expand))))

;; Linting
(use-package flycheck
  :diminish "flycheck"
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Color based on error status

(use-package flycheck-color-mode-line
  :after flycheck
  :config
  (flycheck-color-mode-line-mode))

;; The actual snippets used
(use-package yasnippet-snippets
  :after yasnippet
  :defer 1)

;; Assembly files
(use-package asm-mode
  :mode ("\\.s\\'"))

;; Cmake files
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda () (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\)"
                                  1 font-lock-warning-face t)))))))

;; Better c++14 highlighting
(use-package modern-cpp-font-lock
  :diminish
  :hook (c++-mode .  modern-c++-font-lock-global-mode))

;; Keep keyword font lock
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with font lock
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\)"
                                         1 font-lock-warning-face t))))))

;; flash matching parentheses
(add-hook 'after-init-hook 'show-paren-mode)

;; highlight lines
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Faster commenting
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Delete useless whitespace
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; Org mode ;;;
(use-package org
  :init
  (setq org-log-done 'time
        org-list-demote-modify-bullet t
        org-list-allow-alphabetical t
        org-src-tab-acts-natively t
        org-agenda-files (list "~/org/work.org"
                               "~/org/misc.org"))
  :hook ((org-mode . (lambda () (progn (visual-line-mode) (org-indent-mode)))))
  :mode ("\\.txt\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         (:map org-mode-map
               ("C-c l" . org-store-link)
               ("C-c c" . org-capture)
               ("C-c b" . org-switch)))
  :config
  ;; Change line wrap
  (setq visual-fill-column-width 130))

;; fancy bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package zpresent
  :config)

;;; Programming languages ;;;

(require 'setup-c)
;; (require 'setup-cl);

;;; Asthetics ;;;

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

;; Fix funky mode line problems with solarized package and powerline
(set-face-attribute 'mode-line nil :underline nil :overline nil :box nil
                    :foreground "#999" :background "#222")

(use-package solarized-theme
  :config
  (setq solarized-high-contrast-mode-line t
        solarized-use-variable-pitch t
        solarized-scale-org-headlines nil
        solarized-distinct-fringe-background t)
  (load-theme 'solarized-dark t))

(use-package powerline
  :init
  (setq powerline-default-separator 'arrow)
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
                                         (powerline-buffer-size face0))
                                       (powerline-vc face0 'r)
                                       (funcall separator-left face0 face1)
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-raw "[" face2 'l)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2))
                                       (powerline-raw "]" face2)))

                            (rhs (list (powerline-raw " ;) " face2)
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
  :config
  (my-powerline-theme))

(provide initfile)
;;;; initfile ends here
