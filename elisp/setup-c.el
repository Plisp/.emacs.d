;;; C/C++ IDE

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config
  (setq c-default-style "k&r")
  (setq-default tab-width 2
                indent-tabs-mode t))

(use-package clang-format
  :bind ("C-c f" . clang-format-region))

(use-package ggtags
  :init
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq ggtags-mode-line-project-name nil
        ggtags-oversize-limit 30000000)
  :bind (("M-," . pop-tag-mark)
         :map ggtags-mode-map
         ("C-c g s" . ggtags-find-other-symbol)
         ("C-c g d" . ggtags-find-definition)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g h" . ggtags-view-tags-history)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags))
  :hook ((c++-mode . ggtags-mode)
         (c-mode . ggtags.mode)
         (asm-mode . ggtags-mode)
         (makefile-mode . ggtags-mode)
         (sh-mode . ggtags-mode)
         (java-mode . ggtags-mode)))

(use-package company-c-headers
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2/"))

(defvar my-ycmd-server-command '("python3" "/usr/src/ycmd/ycmd"))
(defvar my-ycmd-extra-conf-whitelist "~/code/configs/.ycm_extra_conf.py")
(defvar my-ycmd-global-config "~/code/configs/.ycm_extra_conf.py")

(use-package ycmd
  :init
  (set-variable 'ycmd-server-command my-ycmd-server-command)
  (set-variable 'ycmd-extra-conf-whitelist 'my-ycmd-extra-conf-whitelist)
  (set-variable 'ycmd-global-config 'my-ycmd-global-config)
  :hook ((c-mode-common . ycmd-mode)
         ;(rust-mode . ycmd-mode)
         )
  :config
  (require 'ycmd-eldoc)
  (ycmd-eldoc-setup))

(use-package company-ycmd
  :after (ycmd company)
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :after (ycmd flycheck)
  :config
  (flycheck-ycmd-setup))

;; This must be the first backend
(add-to-list 'company-backends 'company-c-headers)

(provide 'setup-c)
