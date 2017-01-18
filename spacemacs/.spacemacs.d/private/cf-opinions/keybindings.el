; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)
; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") 'newline-above-point)

; Control s to save
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-visual-state-map (kbd "C-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "C-s") 'save-buffer)
