;;; packages.el --- cf-org layer packages file for Spacemacs.

;;   Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(org-gcal)
  "The list of Lisp packages required by the cf-org layer."
)
(defun cf-org/init-org-gcal ()
  (use-package org-gcal
    :defer t

    :init
      (spacemacs/set-leader-keys "aog" 'org-gcal-sync)
    :config
      (load-file "~/corporate-secrets/credentials.el")
      (cf/set-org-gcal-credentials)
    )
  )

;;; packages.el ends here
