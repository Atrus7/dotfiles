
(defconst cf-py-packages '(python))


  (defun cf-py/post-init-python()
    (use-package python
      :defer t
      :config
      (define-key python-mode-map (kbd "TAB") #'python-indent-shift-right)
      (define-key python-mode-map (kbd "<backtab>") #'python-indent-shift-left)
      ))
