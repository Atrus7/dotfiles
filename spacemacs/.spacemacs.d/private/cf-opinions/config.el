;; Spacemacs is notoriously buggy. Don't let things fall through the cracks

;; TODO do this idiomatically
;; disable swiping for moving backwards and forwards buffers.
;(setq debug-on-error t)

;; Don't edit any shell files except my own
(setq sh-make-vars-local nil)

(setq create-lockfiles nil)

;; always follow link when visiting vc'd symlink
(setq vc-follow-symlinks t)

(setq initial-scratch-message "* Scratch Buffer\n")

(setq doc-view-continuous t)

;; creates a newline without breaking the current line
(defun newline-below-point ()
  "1. Move to end of line
   2. insert newline with indentation"
  (interactive)
  (let((oldpos(point)))
    (end-of-line)
    (newline-and-indent)))

(defun newline-above-point ()
  (interactive)
  (if (eq (line-number-at-pos) 1)
      (progn (beginning-of-line) (newline) (previous-line))
      (progn (previous-line) (newline-below-point))
    )
  )

; TODO: Set to <SPC> bg
(defun get-all-magit-buffers()
  (interactive)
  (helm :sources helm-mini-default-sources
        :buffer "*helm mini*"
        :input "*magit "
        :ff-transformer-show-only-basename nil
        :truncate-lines helm-buffers-truncate-lines)
 )


(defvar cf/scratch-save-dir "~/tmp")
(defun cf/save-scratch-and-file()
  (interactive)
  (spacemacs/switch-to-scratch-buffer)
  (setq scratch_name (concat cf/scratch-save-dir "/" (format-time-string "%m_%d_%y") ".scratch"))
  (set-visited-file-name scratch_name)
  (save-buffer)
  )
