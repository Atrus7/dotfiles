;;; packages.el --- cf-org layer packages file for Spacemacs.

;;   Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(org-gcal org-caldav)
  "The list of Lisp packages required by the cf-org layer."
)
(defun cf-org/init-org-gcal ()
  (use-package org-gcal
    :defer t
    :init
      (require 'org-gcal)
      (spacemacs/set-leader-keys "aog" 'org-gcal-sync)
 ;     :config
    ;  (if (file-accessible-directory-p "~/corporate-secrets")
 ;         (
  ;         (load-file "~/corporate-secrets/credentials.el")
  ;         (cf/set-org-gcal-credentials)
  ;         )
  ;      )
      )
  )

(defun cf-org/init-org-caldav ()
  (use-package org-caldav
    :defer t
    :init
    (require 'org-caldav)
                                        ;(require 'org-gcal)
    (spacemacs/set-leader-keys "aoG" 'org-caldav-sync)
  ;  :config
   ; (if (file-accessible-directory-p "~/corporate-secrets")
   ;     (
   ;      (load-file "~/corporate-secrets/credentials.el")
   ;      (cf/set-org-caldav-credentials)
   ;      )
      )
    )
;;; packages.el ends here
