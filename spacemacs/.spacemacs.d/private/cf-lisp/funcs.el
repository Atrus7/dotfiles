;; useful for testing
(defun cf/eval-and-run()
  (interactive)
  (call-interactively 'eval-defun)
  (let ((fname (intern-soft (current-message))))
    (if (interactive-form fname)
        (call-interactively fname)
        (funcall fname))))
