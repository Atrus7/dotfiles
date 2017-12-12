(defconst cf-opinions-packages '(common-lisp)
  "The list of Lisp packages required by the cf-org layer.")

(defun cf-opinions/post-init-common-lisp()
  (use-package common-lisp
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "," 'lisp-state-toggle-lisp-state)
    )
  )
