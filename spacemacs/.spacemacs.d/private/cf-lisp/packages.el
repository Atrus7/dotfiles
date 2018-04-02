
(defconst cf-lisp-packages '(lisp
                             common-lisp)
  "The list of Lisp packages required by the cf-lisp layer.")

(defun cf-lisp/post-init-lisp()
  (use-package lisp
    :defer t
    :config

    (defconst lisp--prettify-symbols-alist
      '(("lambda"  . ?λ)                  ; Shrink this
        ("."       . ?•)))                ; Enlarge this
    ))

(defun cf-lisp/post-init-common-lisp()
  (use-package common-lisp
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "," 'lisp-state-toggle-lisp-state)
    )
  )
