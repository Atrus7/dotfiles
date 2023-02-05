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

(defun cf/org-num-chapter-format (numbering)
  "Makes number reflect chapter
NUMBERING is a list of numbers."
  (concat "[Ch " (mapconcat #'number-to-string numbering ".") "] "))

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

;; (add-to-list 'pdf-misc-print-program-args "-o sides=two-sided-long-edge")
(setq pdf-misc-print-program-args  '("-o media=a4" "-o fitplot" "-o sides=two-sided-long-edge" "-o print-quality=3"))

(defun cf/print-num-copies ()
  (interactive)
  (let* ((n (read-number "Type a number: " 1))
         (new (append pdf-misc-print-program-args (list (concat "-# " (number-to-string n)))))
         (pdf-misc-print-program-args new))
    (pdf-misc-print-document buffer-file-name)))

;; fixup emdashes
(defgroup typopunct nil
  "Minor mode for typographical punctuation marks."
  :group 'local)

(defconst typopunct-en-dash
  (decode-char 'ucs #x2013))

(defconst typopunct-em-dash
  (decode-char 'ucs #x2014))

(defun typopunct-insert-typographical-dashes ()
  "Insert a dashes, an em-dash or an em-dash."
  (interactive)
  (cond ((eq (char-before) ?-)
	       (delete-char -1)
	       (insert typopunct-em-dash))
	      ((eq (char-before) typopunct-en-dash)
	       (delete-char -1)
	       (insert typopunct-em-dash))
	      (t (insert ?-))))

;; helper that replaces everywhere in buffer while keeping point
(defun full-replace (search replace)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp search replace)
    ))

;; cleanup whitespace
(defun cf/fixup-double-spaces ()
  (interactive)
  (full-replace "[.]  " "\. ")
  (full-replace "[!]  " "! ")
  (full-replace "[?]  " "? ")
  (full-replace "[\"]  " "\" ")
  (delete-trailing-whitespace)
  )

;; Cleanup org buffer, which won't fold two empty newlines before a new heading
;; Also handles useless newlines in blocks
(defun cf/fixup-org-headlines ()
  (interactive)
  (full-replace "\n\n\n+\\*" "\n\n*")
  (full-replace "\n\n+#\\+END" "\n#+END")
  )


;; He said, "This is how you end dialogue," => He said, "This is how you end dialogue."
(defun cf/fixup-comma-quotes-ending-sentence ()
  (interactive)
  (full-replace ",\"$" ".\"")
  )

(defun cf/replace-smart-quotes ()
  (full-replace "“" "\"")
  (full-replace "”" "\"")
  )

(defun cf/cleanup-org-buffer()
    (interactive)
  (cf/fixup-double-spaces)
  (cf/fixup-org-headlines)
  (cf/replace-smart-quotes)
  (cf/fixup-comma-quotes-ending-sentence)
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

(defun cf/narrow-indirect-buffer ()
  "Narrow to an indirect buffer for the day's writing."
  (interactive)
  (let ((old-name  buffer-file-name)
        (new-name (format "*narrowed_%s*" (file-name-nondirectory buffer-file-name))))
    (spacemacs/narrow-to-region-indirect-buffer)
    (rename-buffer new-name t)
    (setq-local evil--jumps-buffer-targets (format "\\*narrowed_.*"))
))

(defun narrow-todays-indirect-buffer ()
  "Narrow to an indirect buffer for the day's writing."
  (interactive)
  (let ((old-name  buffer-file-name)
        (new-name (format "*%s.%s*" (file-name-nondirectory buffer-file-name) (format-time-string "%m_%d_%y"))))
    (spacemacs/narrow-to-region-indirect-buffer)
    (rename-buffer new-name)
))



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


(defun highlight-trouble-words ()
  (interactive)
  (mapc 'highlight-phrase '(" just " " was " " is " " had "
                            " has "))
  ;; (highlight-regexp "just" 'hi-yellow)
  )
