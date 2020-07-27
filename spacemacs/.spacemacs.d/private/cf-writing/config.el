(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(setq visual-fill-column-width 100)

(defvar-local local-word-count nil)

(add-hook 'after-save-hook 'cf/wc-update t)

(defvar writing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    map))

(define-minor-mode cf/writing-mode ()
  :keymap writing-mode-map
  :group 'writing
  :global t

  nil " Writing" '()
  )
