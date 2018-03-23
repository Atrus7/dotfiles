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

(add-hook 'prog-mode-hook
          (lambda()
            (cf/highlight-indent-offset)))

(defvar cf/scratch-save-dir "~/tmp")
(defun cf/save-scratch-and-file()
  (interactive)
  (spacemacs/switch-to-scratch-buffer)
  (setq scratch_name (concat cf/scratch-save-dir "/" (format-time-string "%m_%d_%y") ".scratch"))
  (set-visited-file-name scratch_name)
  (save-buffer)
  )


;; Stop autocompleting numbers
;(push (apply-partially #'cl-remove-if
;                       (lambda (c) (string-match-p "\\`[0-9]+[a-f]+\\'" c)))
;      company-transformers)
