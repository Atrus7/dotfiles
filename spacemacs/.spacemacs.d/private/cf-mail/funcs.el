;; Use I to toggle image
(defun my-render-html-message ()
  (let* ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (buffer-fix-single-newlines)
    (goto-char (point-min))))
