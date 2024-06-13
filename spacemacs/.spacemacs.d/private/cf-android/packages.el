(defconst cf/android-packages '(magit magit-gerrit))

(defun cf/android/init-magit-gerrit ()
  (use-package magit
    :defer t
    ))

(defun cf/android/post-init-magit ()
  (use-package magit
    :defer t
    :config
    ;; don't auto-refresh status buff if not current buff
    (evil-magit-init)

    ;; we want clickable urls in magit-process-mode
    (add-hook 'magit-process-mode-hook 'goto-address-mode)

    (setq
     magit-refresh-status-buffer nil
     ;; remove --graph for performance concerns
     magit-log-arguments '("-n256" "--decorate"))

    (setq-default magit-gerrit-remote "XXXXX")

    (magit-define-popup-action 'magit-push-popup
      ?m
      "Push to gerrit"
      'cf/android/magit-push-to-gerrit)

    (magit-define-popup-action 'magit-push-popup
      ?P
      "git cf/android format"
      'cf/android/git-cf/android-format)

    (add-hook 'git-commit-mode-hook 'yas-minor-mode)

    ;; Don't rely on cache after magit branch changes
    (advice-add 'magit-checkout :after #'run-projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout :after #'run-projectile-invalidate-cache)
    ))
