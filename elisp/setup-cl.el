;;; Common lisp IDE TODO customize

(use-package sly
  :init
  (add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/elpa/sly-*")))
  :config
  (setq sly-lisp-implementations '((roswell ("ros" "-Q" "run") :coding-system utf-8-unix))
        common-lisp-hyperspec-root *my-hyperspec-location*))

(provide 'setup-cl)
