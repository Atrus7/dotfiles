(defun cf/org-archive-done-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree) ; need to move cursor after archiving so it doesn't skip sequential done entries
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))
(defun cf/org-archive-task ()
  "Archive todo task"
  (interactive)
  )
