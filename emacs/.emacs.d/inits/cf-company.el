(require 'cf-evil)
(require 'company)
;;; Completion -- Welcome to the firm.
(company-mode 1)
(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
      (indent-according-to-mode)))

(define-key evil-insert-state-map "\t" 'indent-or-complete)

(define-key company-active-map (kbd "C-j") #'company-select-next)
(define-key company-active-map (kbd "C-k") #'company-select-previous)
(define-key company-active-map (kbd "C-l") #'company-complete)
(add-hook 'after-init-hook 'global-company-mode) ; All the buffers

(el-init-provide)
