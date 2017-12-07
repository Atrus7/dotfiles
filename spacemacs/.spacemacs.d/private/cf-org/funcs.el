(defun cf/org-archive-done-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

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

;; Wrapper for main publishing fn for addendums
(defun cf/org-publish-blog (&rest args)

  ;; actual publishing
  (apply #'org-html-publish-to-html args)

 )
