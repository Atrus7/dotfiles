                                        ; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)
                                        ; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") 'newline-above-point)
