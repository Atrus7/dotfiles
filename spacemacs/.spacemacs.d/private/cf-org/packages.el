;;; packages.el --- cf-org layer packages file for Spacemacs.

;;  Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(org)
  "The list of Lisp packages required by the cf-org layer.")

(defun cf-org/post-init-org()
  (use-package org
    :defer t
    :config
    (add-to-list 'org-src-lang-modes '("scheme" . scheme) )
    (add-to-list 'org-babel-load-languages '(scheme . t))
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (scheme . t) ))
    (setq org-agenda-files (list(mapconcat 'eval '(org-directory "todo.org") "/")))

    ;; remapping pomodoro thing
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "p" 'org-priority)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "p" 'org-agenda-priority)
    ;; remapping over table
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "t" 'cf/org-schedule-today)
    )
  )

;;; packages.el ends here
