;;;; TODO: fix hangs with company completion

(load (expand-file-name "~/quicklisp/slime-helper.el"))    ; path-to-quicklisp/slime-helper.el

(require 'slime-autoloads)

(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix))
      slime-contribs '(slime-fancy slime-asdf slime-company))

(use-package slime-company
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-major-modes slime-mode
        slime-company-complete-in-comments-and-strings t))

(setq slime-default-lisp 'sbcl)

(load "~/quicklisp/clhs-use-local.el" t)

(provide 'setup-cl)
