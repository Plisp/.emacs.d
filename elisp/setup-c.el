;;;; C languages setup ;;;;

(defvar my/ycmd-server-command '("python" "/usr/src/ycmd/ycmd"))
(defvar my/ycmd-extra-conf-whitelist '("/usr/src/ycmd/ycmd/ycm_extra_conf.py"))
(defvar my/ycmd-global-config "/usr/src/ycmd/ycmd/ycm_extra_conf.py")

(use-package cc-mode
  :mode ("\\.tpp\\'" "\\.h\\'")
  :config
  (c-set-offset 'case-label '2)
  (setq compile-command "clang++ -std=c++17 -stdlib=stdc++ -Wall -Wextra"
				c-default-style "k&r"
				c-basic-offset 4))

;;; code formatting

(use-package clang-format :ensure t
  :bind ("C-c u" . clang-format-buffer))

(use-package string-inflection :ensure t
  :bind (("C-c c c" . string-inflection-cycle)))

(use-package smart-tabs-mode :ensure t
  :config
  (smart-tabs-insinuate 'c 'c++))

;;; header completion
(use-package company-c-headers :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/7.3.0/"))

;;; TODO setup IDE later ;;;

(use-package counsel-etags
  :hook (after-save . counsel-etags-virtual-update-tags)
  :bind (("M-." . counsel-etags-find-tag-at-point)
				 ("M-t" . counsel-find-etags-tag)
				 )
  :config
  (add-to-list 'counsel-etags-ignore-directories "build")
	;; No need to scan TAGS file itself
  (add-to-list 'counsel-etags-ignore-filenames '("TAGS" ".clang-format"))
  (setq counsel-etags-max-file-size 1000
				tags-revert-without-query t
				large-file-warning-threshold nil
				counsel-etags-update-interval 300))

(use-package ycmd
  :hook ((c++-mode . ycmd-mode)
				 (ycmd-mode . ycmd-eldoc-setup))
  :config
  (set-variable 'ycmd-server-command my/ycmd-server-command)
  (set-variable 'ycmd-extra-conf-whitelist my/ycmd-extra-conf-whitelist)
  (set-variable 'ycmd-global-config my/ycmd-global-config)
  (setq ycmd-force-semantic-completion t)
  (require 'ycmd-eldoc))

(use-package company-ycmd
  :after ycmd company
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :after ycmd flycheck
  :config
  (flycheck-ycmd-setup))

(provide 'init-c)
