(defconst cf-term-packages '(term)
  "The list of Lisp packages required by the cf-term layer.")

(defun cf-term/post-init-term()
  (use-package term
    :defer t
    :config
    (evil-define-key '(normal insert) term-raw-map
      (kbd "M-x") 'counsel-M-x

      (kbd "M-h") 'evil-window-left
      (kbd "M-j") 'evil-window-down
      (kbd "M-k") 'evil-window-up
      (kbd "M-l") 'evil-window-right)
    )
  )
