(defun cf/vlc-grab-timestamp ()
  "docstring"
  (interactive)
  (org-insert-heading)
  (call-process "~/bin/vlc_control.py" nil (buffer-name))
  )

(defun cf/vlc-toggle-pause ()
  (interactive)
  (call-process "~/bin/vlc_control.py" nil nil)
  )
(defun cf/wc-update ()
  (setq-local local-word-count (count-words (point-min) (point-max)))
  )

(defun add-quotes-to-font-lock-keywords ()
  "highlight strings in a given mode."
  (interactive)
  (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face))))

(defvar cf/writing-dir (expand-file-name "~/dropbox/writing"))
(defun cf/write-today()
  (interactive)
  (setq-local name (concat cf/writing-dir "/unfiled/" (format-time-string "%m_%d_%y") ".org"))
  (find-file name)
  (save-buffer)
  )

;; fixup emdashes
(defgroup typopunct nil
  "Minor mode for typographical punctuation marks."
  :group 'local)

(defconst typopunct-en-dash
  (decode-char 'ucs #x2013))

(defconst typopunct-em-dash
  (decode-char 'ucs #x2014))

(defun typopunct-insert-typographical-dashes ()
  "Insert a dashes, an en-dash or an em-dash."
  (interactive)
  (cond ((eq (char-before) ?-)
	       (delete-char -1)
	       (insert typopunct-en-dash))
	      ((eq (char-before) typopunct-en-dash)
	       (delete-char -1)
	       (insert typopunct-em-dash))
	      (t (insert ?-))))

;; cleanup whitespace
(defun full-replace (search replace)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp search replace)
    ))

(defun cf/fixup-double-spaces ()
  (interactive)
  (full-replace "[.]  " "\. ")
  (full-replace "[!]  " "! ")
  (full-replace "[?]  " "? ")
  (full-replace "[\"]  " "\" ")
  )


(defun cf/display-word (&rest args)
  "Create a buffer for display word instead of using messages.

Slightly hacky to add the word(being defined) into the buffer,
since the rest of the information is generated by define-word,
that may break."
  (interactive)
  (let*
      ((buffer (get-buffer-create "*Define Word*"))
       (word (propertize (substring-no-properties (thing-at-point 'word)) 'face 'bold))
       (header (format "DEFINING: %s \n" word)))

    (set-buffer buffer)
    (erase-buffer)
    (set-buffer-major-mode buffer)
    (insert header)
    (apply 'insert args)
    (display-buffer buffer))
  )

(defun narrow-todays-indirect-buffer ()
  "Narrow to an indirect buffer for the day's writing."
  (interactive)
  (let ((old-name  buffer-file-name)
        (new-name (format "*%s.%s*" (file-name-nondirectory buffer-file-name) (format-time-string "%m_%d_%y"))))
    (spacemacs/narrow-to-region-indirect-buffer)
    (rename-buffer new-name)))


;;; org marginialia
(defun org-marginalia-make-annotation ()
  (interactive)
  (let ((mark-end (region-end)))
    (org-marginalia-mark (region-beginning) (region-end))
    (org-marginalia-save)
    (org-marginalia-open (1- mark-end))
    (end-of-buffer)))


(defun org-marginalia-browse-forward ()
  (interactive)
  (let ((buf (current-buffer)))
    (org-marginalia-next) (org-marginalia-open (point))
    (pop-to-buffer buf nil t)))


(defun org-marginalia-browse-backward ()
  (interactive)
  (let ((buf (current-buffer)))
    (org-marginalia-prev) (org-marginalia-open (point))
    (pop-to-buffer buf nil t)))
