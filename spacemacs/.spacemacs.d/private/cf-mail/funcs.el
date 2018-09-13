;; Use I to toggle image
(defun my-render-html-message ()
  (let* ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (buffer-fix-single-newlines)
    (goto-char (point-min))))

(defun cf/goto-next-header ()
  (interactive)
  (message-next-header)
  (message-beginning-of-header nil))

(defun message-prev-header ()
    "Go to the beginning of the prev header."
    (beginning-of-line)
    (or (eobp) (forward-char 1))
    (not (if (re-search-backward "^[^ \t]" nil t 2)
	           (beginning-of-line)
	         (goto-char (point-max))))
  )

(defun cf/goto-prev-header ()
  (interactive)
  (message-prev-header)
  (message-beginning-of-header nil))

;; Gnus stuff
(defun cf/get-new-news-and-disconnect (&optional arg)
  "Plug in, send, receive, plug out."
  (interactive "P")
  (gnus-group-save-newsrc)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-group-get-new-news arg)
  (gnus-agent-fetch-session)
  (gnus-group-save-newsrc)
  (gnus-agent-toggle-plugged nil))

;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
     	  (set-buffer buffer)
     	  (when (and (derived-mode-p 'message-mode)
     		           (null message-sent-message-via))
     	    (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

;; (defun my-gnus-summary-keys ()
;;   (local-set-key "y" 'gmail-archive)
;;   (local-set-key "$" 'gmail-report-spam))

;; (defun gmail-archive ()
;;   "Archive the current or marked mails.
;; This moves them into the All Mail folder."
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

;; (defun gmail-report-spam ()
;;   "Report the current or marked mails as spam.
;; This moves them into the Spam folder."
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))
;; )
