;;; C/c++ IDE features leveraging ccls TODO setup advanced

(use-package lsp-mode
  :hook ((c-mode . lsp-mode)
         (c++-mode . lsp-mode)))

(use-package company-lsp
  :after (company lsp-mode)
  :config
  (setq company-lsp-enable-recompletion t)
  (add-to-list 'company-backends 'company-lsp))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(defun +ccls/enable ()
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))

(use-package ccls
  :after lsp-mode
  :config
  (setq ccls-executable "/usr/local/src/ccls/Release/ccls")
  (+ccls/enable))

(provide 'setup-c)
