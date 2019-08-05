(defconst cf-cc-packages '(cc-mode))

(defun cf-cc/post-init-cc-mode()
  (use-package cc-mode
    :defer t
    :config

    (c-toggle-auto-newline -1)
    (evil-define-key '(normal insert visual) c++-mode-map
      (kbd "M-=") 'clang-format-buffer)
    (evil-define-key '(normal insert visual) c-mode-map
      (kbd "M-=") 'clang-format-buffer))
  )
