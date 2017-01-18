(defun org-archive-done-tasks ()
  "Archives all done tasks within buffer."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
