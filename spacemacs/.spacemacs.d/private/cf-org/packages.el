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

    (evil-define-key '(normal insert) evil-org-mode-map
      (kbd "<right>") 'org-shiftmetadown
      (kbd "<left>") 'org-shiftmetaup
      (kbd "<up>") 'org-metaup
      (kbd "<down>") 'org-metadown

      (kbd "M-h") 'evil-window-left
      (kbd "M-j") 'evil-window-down
      (kbd "M-k") 'evil-window-up
      (kbd "M-l") 'evil-window-right)
    )
  )

;;; packages.el ends here
