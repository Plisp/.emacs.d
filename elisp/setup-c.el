;;; C/c++ IDE features leveraging ccls TODO setup advanced

;; (setq-local indent-tabs-mode t)
;; (setq-local tab-width 4)
;; (setq-local c-basic-offset 4)
;; (setq-local fill-column 80)

;; Clang format takes care of style control
(use-package clang-format
  :bind ("C-c f" . clang-format-region))

;; Better c++14 highlighting
(use-package modern-cpp-font-lock
  :hook (c++-mode .  modern-c++-font-lock-global-mode))

(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients)
  (setq lsp-auto-guess-root t))

(use-package company-lsp
  :config
  (setq company-lsp-enable-recompletion t)
  (push 'company-lsp company-backends))

(setq ccls-executable "/usr/local/src/ccls/Release/ccls")
(use-package ccls :defer t)
(add-hook 'c++-mode-hook (lambda () (require 'ccls) (lsp)))
