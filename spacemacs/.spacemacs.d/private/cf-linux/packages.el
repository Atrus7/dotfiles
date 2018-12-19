(defconst cf-linux-packages
  '(daemons))


(defun cf-linux/init-daemons ()
  (use-package daemons
    :defer t
    :config
    (setq-default daemons-always-sudo t)

    ;; (with-eval-after-load 'daemons (evilified-state-evilify-map daemons-mode-map))
    ;; (advice-add 'daemons :after (lambda () (evil-evilified-state)))
    )

  )
