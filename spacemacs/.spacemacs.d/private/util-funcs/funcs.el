;;; Functions that don't require any other private functions and might be useful generally.
;;; TODO ensure these functions will be loaded before the rest of cf/* layers

;; move point to previous error. based on code from
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun insert-after-fn (fn)
  (funcall-interactively fn)
  (evil-insert 1))

;; creates a newline without breaking the current line
(defun newline-below-point ()
  "1. Move to end of line
   2. insert newline with indentation"
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun newline-above-point ()
  (interactive)
  (if (eq (line-number-at-pos) 1)
      (progn (beginning-of-line) (newline) (previous-line))
    (progn (previous-line) (newline-below-point))
    )
  )

(defun append-semicolon ()
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (insert ";")
    (goto-char pos)
    )
  )

(defun reload-buffer ()
  "Best way to repaint a buffer.
   Saves, kills, and reopens the buffer."
  (interactive)
  (let ((cur-buffer (buffer-file-name)))
    (if (not cur-buffer)
        (message "Buffer cannot be revisited... won't kill it.")
      (progn
        (save-buffer)
        (kill-buffer)
        (find-file cur-buffer)
        (recenter nil))
      )
    )
  )

(defun buffer-fix-single-newlines()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\n[:blank:]*\n[:blank:]*\n+" (buffer-end 1) t)
    (replace-match "\n\n")))
