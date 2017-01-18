;; Spacemacs is notoriously buggy. Don't let things fall through the cracks

;; TODO do this idiomatically
;; disable swiping for moving backwards and forwards buffers.
;(setq debug-on-error t)

;; Don't edit any shell files except my own
(setq sh-make-vars-local nil)

(setq create-lockfiles nil)

(setq initial-scratch-message "*Scratch Buffer*\n")

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

;(defun save-scratch-and-file() {}) 
