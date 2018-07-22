(when (spacemacs/system-is-linux)
  (setq scheme-program-name "guile"
        geiser-default-implementation 'guile
        geiser-mode-eval-last-sexp-to-buffer t))
