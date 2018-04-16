;;; General functions

(defun cf/insert-after-fn (fn)
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

;;; Specific cf functions
(defun cf/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((select-enable-clipboard t))
        (kill-new filename))
      )))

;; highlight non-aligning indent offset
(defun cf/highlight-indent-offset ()
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(lambda (limit)
         (re-search-forward
          (format "^ \\{%d\\}*\\( \\{1,%d\\}\\)[^ ]" tab-width (- tab-width 1))
          limit t))
      1 'whitespace-trailing))))

(defun cf/save-scratch-and-file()
  (interactive)
  (spacemacs/switch-to-scratch-buffer)
  (setq scratch_name (concat cf/scratch-save-dir "/" (format-time-string "%m_%d_%y") ".scratch"))
  (set-visited-file-name scratch_name)
  (save-buffer))

(defun cf/get-projectile-magit ()
  "Hacky...temporarily sets  projectile-switch-project-action and then unsets it"
  (interactive)
  (setq projectile-switch-project-action 'projectile-vc
        counsel-projectile-switch-project-action 'projectile-vc ;;ivy supp
        )
  (counsel-projectile-switch-project)
  (setq projectile-switch-project-action 'projectile-find-file
        counsel-projectile-switch-project-action 'projectile-find-file ;;ivy supp
        ))

(defun cf/find-private-layers ()
  "shortcut to private layers dir"
  (interactive)
  (counsel-find-file "~/dotfiles/spacemacs/.spacemacs.d/private/"))

(defun cf/find-org-files ()
  "shortcut to private layers dir"
  (interactive)
  (counsel-find-file "~/org/"))

;; TODO: hungry delete for lines??
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun cf/chrome-linux-ident (region-start region-end)
  ;;; Look up identifier in linux kernel
  (interactive "r")
  (let ((url (concat
              "https://elixir.bootlin.com/linux/v4.9/ident/"
              (cf/get_region_or_word))))
    (browse-url url)))

(defun cf/chrome-google ()
  ;;; Look up identifier in google
  (interactive)
  (let ((url (concat
              "www.google.com/search?q="
              (cf/get_region_or_point))))
    (browse-url url)))

(defun cf/get_region_or_point ()
  ;;; Get the currently highlighted region or the symbol at point
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end) )
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds)))
      (if (null start)
          ""
        (buffer-substring-no-properties start end)))))

(defun cf/describe-last-function()
  (interactive)
  (describe-function last-command))


(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(defun cf/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun cf/highlight-logs ()
  "Highlight certain lines in specific files.  Currently, only log files are supported."
  (interactive)
  (hi-lock-mode 1)
  (highlight-lines-matching-regexp "ERROR" 'hi-red-b)
  (highlight-lines-matching-regexp "NOTE" 'hi-blue-b))
