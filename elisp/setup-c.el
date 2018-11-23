;;; C/c++ IDE features leveraging ccls TODO setup advanced

(use-package lsp-mode)

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
	:hook ((c-mode . +ccls/enable)
				 (c++-mode . +ccls/enable))
	:config
	(setq ccls-executable "/usr/local/src/ccls/Release/ccls"))

(provide 'setup-c)
