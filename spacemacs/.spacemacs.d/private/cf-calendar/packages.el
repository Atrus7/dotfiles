;;; packages.el --- cf-calendar layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <atrus@archy>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cf-calendar-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cf-calendar/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cf-calendar/pre-init-PACKAGE' and/or
;;   `cf-calendar/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cf-calendar-packages
  '(calfw
    calfw-org)
)

(defun cf-calendar/init-calfw ()
  "Initialize calfw and add key-bindings"
  (use-package calfw
    :defer t
    :commands (cfw:open-calendar-buffer)
    :config
    (progn
      (define-key cfw:calendar-mode-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:calendar-mode-map (kbd "TAB") 'cfw:show-details-command)
      (define-key cfw:calendar-mode-map (kbd "C-j") 'cfw:navi-next-item-command)
      (define-key cfw:calendar-mode-map (kbd "C-k") 'cfw:navi-prev-item-command))))

(defun cf-calendar/init-calfw-org ()
  "Initialize calfw-org and add key-bindings"
  (use-package calfw-org
    :defer t
    :commands (cfw:open-org-calendar)
    :init
    (spacemacs/set-leader-keys "aC" 'cfw:open-org-calendar)
    :config
    (progn
      (define-key cfw:org-schedule-map (kbd "d") 'cfw:change-view-day)
      (define-key cfw:org-schedule-map (kbd "w") 'cfw:change-view-week)
      (define-key cfw:org-schedule-map (kbd "m") 'cfw:change-view-month)
      (define-key cfw:org-schedule-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:org-schedule-map (kbd "TAB") 'cfw:org-open-agenda-day)
      (define-key cfw:org-custom-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:org-custom-map (kbd "TAB") 'cfw:org-open-agenda-day)

      (setq cfw:org-capture-template '("d" "calfw2org" entry (file ,(cf/get-orgfiles-path  "diary.org"))
                                       "* %? \n SCHEDULED: %(cfw:org-capture-day)"))

      )))


;;; packages.el ends here
