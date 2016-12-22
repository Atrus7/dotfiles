(add-to-list 'org-src-lang-modes '("scheme" . scheme) )
(add-to-list 'org-babel-load-languages '(scheme . t))
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (scheme . t) ))

(when (spacemacs/system-is-linux)
  (setq scheme-program-name "guile")
  )
