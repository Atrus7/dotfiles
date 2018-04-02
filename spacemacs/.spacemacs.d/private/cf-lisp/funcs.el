;; useful for testing
(defun cf/eval-and-run()
  (interactive)
  (call-interactively 'eval-defun)
  (let ((fname (intern-soft (current-message))))
    (if (interactive-form fname)
        (call-interactively fname)
        (funcall fname))))

(defun current-sexp ()
  "Returns the _current expression_ based on the position of the
  point within or on the edges of an s-expression."
  (cond
   ((looking-at "(") (sexp-at-point))
   ((looking-back ")" 1) (elisp--preceding-sexp))
   (t (save-excursion
        (search-backward "(")
        (sexp-at-point)))))


(defun eval-and-comment-output ()
  "Add the output of the `current-sexp' as a comment at the end
of the line. Calling this multiple times replaces the comment
with the new evaluation value."
  (interactive)
  (let* ((marker " ; -> ")
         (expression (current-sexp))
         (results (eval expression)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward marker (line-end-position) t)
          (delete-region (point) (line-end-position))
        (end-of-line)
        (insert marker))
      (condition-case nil
          (princ (pp-to-string results) (current-buffer))
        (error (message "Invalid expression"))))))
