;; remapping pomodoro thing
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "p" 'org-priority)
(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "p" 'org-agenda-priority)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "s" 'cf/org-schedule-today) ; remapping over org-schedule
