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