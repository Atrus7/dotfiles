(require 'cf-evil)
(require 'magit)

(require 'evil-magit)

;;; Magit - The git genie
(setq magit-last-seen-setup-instructions "1.4.0"
      magit-auto-revert-mode nil
      magit-diff-use-overlays nil
      magit-use-overlays nil
      )

;; Evil-magit
;; Start to insert mode when editing commit messages
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

(el-init-provide)
