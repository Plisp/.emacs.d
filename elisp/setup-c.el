;;;; C/C++ IDE

;; Tagging
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
    ("u" ggtags-update-tags)))

;; Fuzzy intelligent completion
(use-package ycmd
  :init
  (set-variable 'ycmd-server-command '("python" "/usr/local/src/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "~/global_conf.py")
  (set-variable 'ycmd-extra-conf-whitelist "~/global_conf.py")
  :hook ((c-mode . ycmd-mode)
         (c++-mode . ycmd-mode))
  :config
  (require 'ycmd-eldoc)
  (ycmd-eldoc-setup)
  ;; Integration with company
  (use-package company-ycmd
    :after (ycmd company)
    :config (company-ycmd-setup))
  ;; Error checking integration
  (use-package flycheck-ycmd
    :after (ycmd flycheck)
    :config (flycheck-ycmd-setup))
  ;; As of writing this is still vaporware
  (use-package ivy-ycmd
    :after (ycmd ivy)))

;; Header file completion
(use-package company-c-headers
  :after ycmd
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2/")
  (add-to-list 'company-c-headers-path-user "/usr/local/include/")
  ;; This MUST be the first backend
  (add-to-list 'company-backends 'company-c-headers))

(provide 'setup-c)

;;; setup-c ends here
