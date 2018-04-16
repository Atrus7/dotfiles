(defconst cf-term-packages '(multi-term)
  "The list of Lisp packages required by the cf-term layer.")

(defun cf-term/post-init-term()
  (use-package multi-term
    :defer t
    :config
    ;;TODO Test out term-bind-key-alist ?
    (evil-define-key '(normal insert) term-raw-map
      (kbd "M-x") 'counsel-M-x

      (kbd "M-h") 'evil-window-left
      (kbd "M-j") 'evil-window-down
      (kbd "M-k") 'evil-window-up
      (kbd "M-l") 'evil-window-right)

    ;; (add-hook 'term-mode-hook
    ;;           (lambda ()
    ;;             (setq show-trailing-whitespace nil)))

    )
  )
