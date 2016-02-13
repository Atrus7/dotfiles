;;; cf.el --- Collection of functions that I wrote or pulled from others, not belonging to a bigger library
;;; Commentary:

(defun hack-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Hack" 7 size))
(defun meslo-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Meslo LG S" 7 size))
(defun menlo-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Menlo" 7 size))
(defun bitstream-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Bitstream Vera Sans Mono" 7 size))
(defun courier-face ()
  (interactive)
  (set-frame-font "-adobe-courier-medium-r-normal-*-*-*-100-100-*-60-iso10646-1"))

(defun cf/set-frame-font (face default &optional size) ;; pulled from hershel bhave
;(default-frame-alist '((font . "Inconsolata-dz-15")))

  (set-frame-font
   (concat face " "
           (if size
               (if (numberp size) (number-to-string size) (size))
             (if (numberp default) (number-to-string default) (default))))))
(defun cf/configure-os-linux ()
    (cond
     ((find-font (font-spec :name "Hack"))
      (hack-face))
     ((find-font (font-spec :name "Menlo"))
      (menlo-face))
     ((find-font (font-spec :name "Meslo LG S"))
      (meslo-face))
     ((find-font (font-spec :name "Bitstream Vera Sans Mono"))
      (bitstream-face))
     ((find-font (font-spec :name "-adobe-courier-medium-r-normal-*-*-*-100-100-*-60-iso10646-1"))
      (courier-face)))
  )
(defun cf/move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(provide 'cf)
;;; cf.el ends here
