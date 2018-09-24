;;; C/C++ IDE

(use-package cc-mode
  :bind ("C-M-o" . ff-find-other-file)
  :config
	;; Local variables
	(setq indent-tabs-mode t
				tab-width 2
        fill-column 80
				c-basic-offset 2
        ff-search-directories '("." "../src/*" "../include/*" "/usr/include/*" "/usr/local/*/src/*")))

;; Clang format takes care of style control
(use-package clang-format
  :bind ("C-c f" . clang-format-region))

(use-package ggtags
  :init
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq ggtags-mode-line-project-name nil
        ggtags-oversize-limit 30000000)
  :bind (("M-," . pop-tag-mark))
  :hook ((c++-mode . ggtags-mode)
         (c-mode . ggtags-mode)
         (asm-mode . ggtags-mode)
         (makefile-mode . ggtags-mode)
         (sh-mode . ggtags-mode))
  :config
  (defhydra hydra-ggtags (:timeout 3)
    "A menu of useful ggtags functions"
    ("f" ggtags-find-file)
    ("s" ggtags-find-other-symbol)
    ("d" ggtags-find-definition)
    ("r" ggtags-find-reference)
    ("h" ggtags-view-tags-history)
    ("c" ggtags-create-tags)
    ("u" ggtags-update-tags))

  (define-key c-mode-base-map (kbd "<C-i>") 'hydra-ggtags/body))

;; TODO write a hydra for other things to do in c-mode-base-map

(use-package company-c-headers
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2/"))

;; (defvar my-ycmd-server-command '("python3" "/usr/src/ycmd/ycmd"))
;; (defvar my-ycmd-extra-conf-whitelist "~/code/configs/.ycm_extra_conf.py")
;; (defvar my-ycmd-global-config "~/code/configs/.ycm_extra_conf.py")

;; (use-package ycmd
;;   :init
;;   (set-variable 'ycmd-server-command my-ycmd-server-command)
;;   (set-variable 'ycmd-extra-conf-whitelist 'my-ycmd-extra-conf-whitelist)
;;   (set-variable 'ycmd-global-config 'my-ycmd-global-config)
;;   :hook ((c-mode-common . ycmd-mode)
;;          (rust-mode . ycmd-mode))
;;   :config
;;   (require 'ycmd-eldoc)
;;   (ycmd-eldoc-setup))

;; (use-package company-ycmd
;;   :after (ycmd company)
;;   :config
;;   (company-ycmd-setup))

;; (use-package flycheck-ycmd
;;   :after (ycmd flycheck)
;;   :config
;;   (flycheck-ycmd-setup))

;; This one's new TODO keep an eye out for features
;; (use-package ivy-ycmd
;;   :after (ycmd ivy))

;; This must be the first backend
(add-to-list 'company-backends 'company-c-headers)

(provide 'setup-c)

;;; setup-c ends here
