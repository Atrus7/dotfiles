(defconst cf-opinions-packages '(projectile spaceline popwin)
  "The list of Lisp packages required by the cf-opinions layer.")

(defun cf-opinions/post-init-projectile()
  (use-package projectile
    :defer t
    :config
    (setq projectile-mode-line
          '(:eval
            (if (spacemacs/system-is-linux)
                (format " Projectile[%s]" (projectile-project-name))
              " Projectile[remote]") ;; don't slow down over tramp..
            )
          )

    (if (spacemacs/system-is-linux)
        (setq projectile-enable-caching nil)
      (setq
       projectile-file-exists-remote-cache-expire (* 30 60)
       projectile-enable-caching t
       ))
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
    )
  )


(defun cf-opinions/post-init-spaceline()
  (use-package spaceline
    :defer t
    :config
    (spaceline-config)
  )
  )

(defun cf-opinions/post-init-popwin()
  (use-package popwin
    :defer t
    :config
    (push '("*Define Word*" :dedicated t :position bottom :stick t :noselect t :height 0.3) popwin:special-display-config)
    )
  )
