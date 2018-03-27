;; Spacemacs is notoriously buggy. Don't let things fall through the cracks

;; TODO do this idiomatically
;; disable swiping for moving backwards and forwards buffers.
;(setq debug-on-error t)

;; Don't edit any shell files except my own
(setq sh-make-vars-local nil
      shell-default-shell 'shell
      )

(setq create-lockfiles nil)

;; always follow link when visiting vc'd symlink
(setq vc-follow-symlinks t)

(setq initial-scratch-message "* Scratch Buffer\n")

(setq doc-view-continuous t)


(add-hook 'prog-mode-hook
          (lambda()
            (cf/highlight-indent-offset)))

(defvar cf/scratch-save-dir "~/tmp")

;; prefer 115200 baud
(setq serial-speed-history
      (list  "115200";; Given twice because "115200" b/s is the most common speed
             "1200" "2400" "4800" "9600" "14400" "19200"
             "28800" "38400" "57600"   "115200"))

;; Stop autocompleting numbers
;(push (apply-partially #'cl-remove-if
;                       (lambda (c) (string-match-p "\\`[0-9]+[a-f]+\\'" c)))
;      company-transformers)

(setq whitespace-style '(face spaces tabs space-mark tab-mark))
(add-hook 'makefile-mode-hook
          (lambda () (whitespace-mode)))


(spacemacs|define-custom-layout "gnus"
  :binding "g"
  :body
  (gnus))

(spacemacs|define-custom-layout "serials"
  :binding "s"
  :body
  (dolist (path (directory-files "/dev/" nil "ttyUSB.*") nil)
    (serial-term path 115200)
    (split-window-horizontally)))

(spacemacs|define-custom-layout "org"
  :binding "o"
  :body
  (find-file "~/org/todo.org")
  (split-frame-vertically)
  (org-todo-list))
