;;; Flycheck - It's perfect when my fingers aren't.
;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode 'flycheck-mode)

;; Python linting 80 chars too rough
(setq flycheck-flake8-maximum-line-length 119)

;; Spell checking :)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(el-init-provide)
