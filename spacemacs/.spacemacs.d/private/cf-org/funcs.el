(defun cf/org-archive-done-and-NA-tasks ()
  "Archives all done and NA tasks within buffer."
  (interactive)
  (mapc 'cf/org-archive-task-keyword '("/DONE" "/NA"))
  )
(defun cf/org-archive-task-keyword (keyword)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   keyword 'file))


(defun cf/org-archive-NA-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   "/NA" 'file)
  )

(defun cf/wc-helper ()
  "Counts the number of words in the region and adds it to the kill-ring."
  (save-excursion
    (goto-char (point-min))
    (kill-new (number-to-string (count-matches "\\sw+")))))

;; Ugly hack, make this better
(defun cf-clock-out (args)
  "Custom to get word count"
  (interactive "P")
  (cf/org-clock-out)
  (call-interactively 'org-clock-out))
(defun cf-clock-in (args)
  (interactive "P")
  (cf/org-clock-in)
  (call-interactively 'org-clock-in))

(defun cf/export-to-org ()
  (let ((org-export-with-toc nil)
        (org-export-with-title nil)
        (org-export-with-author nil)
        (org-export-time-stamp-file nil)
        (org-export-with-date nil)
        (org-export-show-temporary-export-buffer nil)
        )

    (org-org-export-as-org)))

(defun cf/org-count-exported-words ()
  "This function exports the current buffer temporarily and runs word-count over the exported content. Gets the word count as a kill and prints it out."
  (interactive)
  (cf/export-to-org)
  (cf/run-on-org-export 'cf/wc-helper)
  (message (format "Exported content has %s words." (car kill-ring)))
  (string-to-number (car kill-ring))
  )

(defun cf/run-on-org-export (f)
  (save-window-excursion
    (switch-to-buffer "*Org ORG Export*")
    (funcall f) ;; puts word count on killring
    )
  )

(defun cf/org-count-exported-pages ()
  (interactive)
  (let* ((wperpage 300.0)
         (words (cf/org-count-exported-words)) )

    (message (format "Exported content has %s pages." (/ words wperpage)))))

(defun cf/git-count-uncommitted-words ()
  (interactive)
  (message (format "Uncommitted changes have %s words."
                   (replace-regexp-in-string "\n$" ""
                                             (shell-command-to-string "~/bin/git_word_count.sh git_words_diff"))))
  )

(defvar word-count-exported-in 0 "Word count of exported content only.")
(defun cf/org-clock-in ()
  (setq word-count-exported-in (cf/org-count-exported-words))
  )

(defvar word-count-exported-out 0 "Word count of exported content only.")
(defun cf/org-clock-out ()
  (setq word-count-exported-out (cf/org-count-exported-words))
  )

(defun cf/org-clock-out-log ()
  (if (string-equal org-log-note-purpose "clock-out")
      (progn
        (insert (format "Words written: %d \n Total Word Count: %d."
                        (- word-count-exported-out word-count-exported-in)
                        word-count-exported-out
                        ))
        (org-ctrl-c-ctrl-c))) ;; Wrap up note
  )

(defun helm-files-insert-as-static-link (candidate)
  (insert (format "%s" (replace-regexp-in-string "^.*/static/" "http:/static/" candidate))))

(defun helm-ff-run-insert-blog-img ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-files-insert-as-static-link)))


;; Insert [ ] to make a checkboxed list
(fset 'insert-checkbox-at-line
      [?m ?q ?0 ?f ?- ?a ?  ?\[ ?  ?\] escape ?` ?q])

;; Requires "highlight" to be in search buffer
(fset 'fix-highlight-format-ebook-export
      [?n ?d ?f ?- ?x ?j ?j ?v ?i ?p ?h ?h ?l ?v ?s ?\" ?k ?k ?J ?J ?i ?: return escape ?x])

;; IFTTT files org-captures into .org.txt files, so we want to refile all of those entries into their respective org files
(defun cf/org-append-dictated-captures ()
  "Moves .org.txt entries into respective .org files and deletes .org files"
  (interactive)
  (setq org-txt-extension ".org.txt")
  (setq org-txt-files (directory-files org-directory nil ".org.txt"))
  (setq files-to-append
        (mapcar
         (lambda (filename)
           (substring filename 0
                      (- (length filename) (length org-txt-extension))))

         org-txt-files
         ))

  (mapc 'cf/org-file-org-txt files-to-append)
  )

(defun cf/org-schedule-today ()
  "Checks if heading is scheduled and if not schedules for today"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (if (re-search-forward
         org-scheduled-time-regexp
         (save-excursion (outline-next-heading) (point)) t)
        (message  "Already scheduled")
      (progn
        (newline-below-point)
        (insert "SCHEDULED: ")
        (org-insert-time-stamp (current-time))
        (message  "Scheduled for today"))
      )
    )
  )

(defun cf/get-orgfiles-path (fname)
  (let ((file_path (concat "~/org/" fname)))
    (if (and at-work (not (spacemacs/system-is-linux)))
        ;; tramp around
        (cf/get-desk-path file_path)
      ;;otherwise just let org figure it out.
      fname
      )))


(defun cf/org-file-org-txt (filename)
  "Append [filename].org.txt contents to [filename].org file in the [dir], then delete the .org.txt"
  (setq org-filename (concat filename ".org"))
  (setq org-txt-filename (concat org-filename ".txt"))

  (with-temp-buffer
    (cd org-directory) ; brittle but simple, only works in the main org folder
    (if (and (file-exists-p org-filename) (file-writable-p org-filename) (file-readable-p org-txt-filename))
        (progn
          (newline) ; don't start append to any existing line
          (insert-file-contents org-txt-filename)
          (append-to-file (point-min) (point-max) org-filename)
          (delete-file org-txt-filename)
          (message "Filed and removed" org-txt-filename)
          )
      nil
      )))
(defun cf/pass (&rest ignore_args)
  "acts like python pass"
  )

;; TODO : this is a broader function. Refactor as such
;; Returns a file's text as a string
(defun cf/org-pull-template-from-file (template_file_path)
  (with-temp-buffer
    (if (and (file-exists-p template_file_path) (file-readable-p template_file_path))
        (progn
          (insert-file-contents template_file_path)
          (buffer-string)
          )
      nil
      )))

(defun cf/org-publish-file (&rest args)
  (interactive)
  (org-publish-current-file t)
  (browse-url-chrome (format "%s"
                             (replace-regexp-in-string
                              ".org" ".html"
                              (replace-regexp-in-string "^.*/posts/" "https://christopherfin.com/" buffer-file-name))))
  )

;; Wrapper for main publishing fn for addendums
(defun cf/org-publish-blog (&rest args)

  ;; actual publishing
  (apply #'org-html-publish-to-html args)
  )

;; Pulled from https://github.com/howardabrams/dot-files
(defun cf/align-variables (start end)
  "Attempts to align all variables in an assignment list or keys
in a hash table. For instance:

  (\"org-mode\"
   :base-extension \"org\"
   :recursive t
   :headline-levels 4  ; Just the default for this project.
   :auto-sitemap t     ; Generate sitemap.org automagically
  )

Turns into the following if the region begins on the first line
with the colon:

  (\"org-mode\"
    :base-extension  \"org\"
    :recursive       t
    :headline-levels 4  ; Just the default for this project.
    :auto-sitemap    t     ; Generate sitemap.org automagically
  )

Note: This currently does not align the comments.

All lines in region will be indented to the position of the first
line. For most languages/modes, this should be sufficient, but if
it doesn't work, start the region as the column they should all
be indented. For instance:

   var x = 10,
       start = beginningOfFile,
       end = File.end();

Start the region at the x, to achieve:

   var x     = 10,
       start = beginningOfFile,
       end   = File.end();"
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let* ((times (count-lines start end))
           (real-start (if (looking-at-p "[ \\t(]")
                           (1- (search-forward-regexp "[^ \\t(]" end t))
                         start))
           (real-end nil)  ;; Will be set later
           (dest-column (progn
                          (goto-char real-start)
                          (current-column))))

      ;; Step 1. Align all lines to the column of the text in the first line
      (dotimes (line times)
        (forward-line)
        (indent-line-to dest-column))
      (setq real-end (point))

      ;; Step 2. Align all the values in a second column
      (align-regexp real-start real-end "\\(\\s-*\\)\\(\\S-*\\)\\(\\s-*\\)" 3 1 nil))))

(defun cf/unhighlight-region ()
  (interactive)
  (if (string-equal evil-state "visual")
      (progn
        (hlt-unhighlight-region)
        (evil-exit-visual-state))

    (hlt-unhighlight-symbol (symbol-at-point))))

(defun cf/highlight-region ()
  (interactive)
  (if (string-equal evil-state "visual")
      (progn
        (hlt-highlight-region)
        (evil-exit-visual-state))

    (hlt-highlight-symbol (symbol-at-point))))

(defun cf/get-notes-highlights-link ()
  (interactive)
  (let ((org-link-file-path-type 'absolute)
        (file (concat "file:"
                      (abbreviate-file-name
                       buffer-file-name))))
    (org-insert-link t file "Notes / Highlights")
    ))

;; https://emacs.stackexchange.com/questions/3949/fixing-auto-capitalize-to-work-with-org-mode-headings-and-lists
(defun org-auto-capitalize-headings-and-lists ()
  "Create a buffer-local binding of sentence-end to auto-capitalize
section headings and list items."
  (make-local-variable 'sentence-end)
  (setq sentence-end (concat (rx (or
                                  ;; headings
                                  (seq line-start (1+ "*") (1+ space))
                                  ;; list and checklist items
                                  (seq line-start (0+ space) "-" (1+ space) (? (or "[ ]" "[X]") (1+ space)))))
                             "\\|" (sentence-end))))

(add-hook 'org-mode-hook #'org-auto-capitalize-headings-and-lists)

(defun org-time-today ()
  "Time in seconds today at 0:00.
Returns the float number of seconds since the beginning of the
epoch to the beginning of today (00:00)."
  (float-time (apply 'encode-time
                     (append '(0 0 0) (nthcdr 3 (decode-time))))))

;; From https://github.com/fniessen/emacs-leuven/blob/master/org-leuven-agenda-views.txt
(defun cf/skip-entry-unless-overdue-deadline ()
  "Skip entries that have no deadline, or that have a deadline later than or equal to today."
  (let* ((dl (org-entry-get nil "DEADLINE")))
    (if (or (not dl)
            (equal dl "")
            (org-time>= dl (org-time-today)))
        (progn (outline-next-heading) (point)))))

(defun cf/org-find-and-narrow-to-subtree ()
  "Search a subtree and narrow to that subtree"
  (interactive)
  (widen)
  (org-goto)
  (org-narrow-to-subtree)
  )

(defun cf/html2org-paste ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "xclip -selection clipboard -o -t text/html | pandoc -f html -t json | pandoc -f json -t org --wrap none"))
  (yank))

(defun cf/org2html-copy ()
  "Convert the contents of the current buffer or region from Org
mode to HTML.  Store the result in the clipboard."
  (interactive)
  (cf/export-to-org)
  (cf/run-on-org-export
   (lambda ()(shell-command-on-region (point-min)
                                      (point-max)
                                      "orgtoclip"))))

(defun cf/org2html-copy-fast ()
  "Skip the org-export, just punch straight to html copy"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning)
                               (region-end)
                               "orgtoclip")
    (shell-command-on-region (point-min)
                             (point-max)
                             "orgtoclip")))

;; Modified org-publish-find-date function, to get date from file, rather than
;; waiting. Used to determine order.
;; (defun org-publish-find-date (file project)
(defun cf/org-publish-find-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
  (if (stringp project) (let ((project (assoc project org-publish-project-alist)))


 ;
	(org-publish-cache-set-file-property
	 file :date
	 (let ((date (org-publish-find-property file :date project)))
	   ;; DATE is a secondary string.  If it contains
	   ;; a time-stamp, convert it to internal format.
	   ;; Otherwise, use FILE modification time.
	   (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
		          (and ts
			             (let ((value (org-element-interpret-data ts)))
			               (and (org-string-nw-p value)
				                  (org-time-string-to-time value))))))
		       ((file-exists-p file)
		        (file-attribute-modification-time (file-attributes file)))
		       (t (error "No such file: \"%s\"" file))))))))


(defun cf/unschedule ()
  "Remove any deadline and schedule from this entry"
  (interactive)
  ;; Calling with universal argument(4) cancels it.
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (org-agenda-schedule '(4))
        (org-agenda-deadline '(4)))
    (progn
      (org-schedule '(4))
      (org-deadline '(4)))
    )
  (message "Schedule/Deadline timestamps cleared.")
  )

;; https://stackoverflow.com/questions/18076328/org-mode-export-to-latex-suppress-generation-of-labels
(defun latex-remove-org-mode-labels (text backend info)
  "Org-mode automatically generates labels for headings despite explicit use of `#+LABEL`. This filter forcibly removes all automatically generated org-labels in headings."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\\label{sec:org[a-f0-9]+}\n" "" text)))

;; (add-to-list 'org-export-filter-headline-functions
;;              'latex-remove-org-mode-labels)


(defun cf/org-publish-sitemap-custom (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (let ((filtered-list (remove '("[[file:about.org][About]]") list)))

  (concat
   "#+OPTIONS: html-link-use-abs-url:nil html-scripts:t  html5-fancy:nil tex:t title:nil \n\n"
   "#+TITLE: " title "\n\n"
   "# Don't modify this file. It's auto-generated. Instead, modify cf/org-publish-sitemap-custom.\n"
   "#+ATTR_HTML: :class AuthorImage \n"
   "https:/static/post-imgs/author-img.jpg \n\n"
          (org-list-to-org filtered-list)))
  )


;; ((string-equal entry "about.org") "")
(defun cf/org-publish-entry-custom (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond
   ((not (directory-name-p entry))
	       (format "[[file:%s][%s]]"
		             entry
		             (org-publish-find-title entry project)
		             ;; (org-publish-find-date entry project)
                 ))
	      ((eq style 'tree)
	       ;; Return only last subdir, capitalized.
	       (capitalize (file-name-nondirectory (directory-file-name entry))))
	      (t entry)
))


;; overriding function from org-html
;; basically just want to avoid wrapping images in <p> tags...
(defun org-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute."
  (let ((html5-fancy (org-html--html5-fancy-p info)))
    (format (if html5-fancy "\n<figure%s>\n%s%s\n</figure>"
	            "\n<div%s class=\"figure\">\n%s%s\n</div>")
	          ;; ID.
	          (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
	          ;; Contents.
	          (if html5-fancy contents (format "%s" contents))
	          ;; Caption.
	          (if (not (org-string-nw-p caption)) ""
	            (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			                  "\n<p>%s</p>")
		                  caption)))))
