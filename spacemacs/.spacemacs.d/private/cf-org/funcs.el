(defun cf/org-archive-done-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  )

(defun helm-files-insert-as-static-link (candidate)
  (insert (format "%s" (replace-regexp-in-string "^.*/static/" "http:/static/" candidate))))

(defun helm-ff-run-insert-blog-img ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-files-insert-as-static-link)))

(defun cf/org-archive-NA-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   "/NA" 'file)
  )

;; Insert [ ] to make a checkboxed list
(fset 'insert-checkbox-at-line
      [?m ?q ?0 ?f ?- ?a ?  ?\[ ?  ?\] escape ?` ?q])

;; Requires "highlight" to be in search buffer
(fset 'fix-highlight-format-ebook-export
   [?n ?d ?f ?- ?x ?j ?j ?v ?i ?p ?h ?h ?l ?v ?s ?\" ?k ?k ?J ?J ?i ?: return escape ?x])


;; TODO implement
;; (defun cf/org-archive-task ()
;;   "Archive todo task"
;;   (interactive)
;;   )


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
      file_path
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
  (org-publish-current-file)
  (browse-url-chrome (format "%s"
                             (replace-regexp-in-string
                              ".org" ".html"
                              (replace-regexp-in-string "^.*/posts/" "http://syscowboy.com/" buffer-file-name))))
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
