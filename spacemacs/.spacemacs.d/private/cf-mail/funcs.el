;; Use I to toggle image
(defun my-render-html-message ()
  (let* ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (buffer-fix-single-newlines)
    (goto-char (point-min))))

(defun cf-mail-compose-setup ()
  (setq-local mu4e-compose-format-flowed t)
  (setq-local use-hard-newlines -1)
  (turn-off-auto-fill))
