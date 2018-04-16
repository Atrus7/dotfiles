;; Spacemacs is not opinionated enough or not enough like me.

;; The place to save all my scratch buffers I end up keeping...
(defvar cf/scratch-save-dir "~/tmp")

(setq sh-make-vars-local nil ; Don't edit any shell files except my own
      shell-default-shell 'shell
      create-lockfiles nil
      vc-follow-symlinks t
      initial-scratch-message "* Scratch Buffer\n"
      doc-view-continuous t)
(setq
 ivy-fixed-height-minibuffer t
 ivy-height 14
 ivy-initial-inputs-alist nil
 ivy-count-format "%-4d "
 )

(add-hook 'focus-out-hook 'save-all)
(add-hook 'prog-mode-hook
          (lambda ()
            (cf/highlight-indent-offset)
            (hungry-delete-mode)
            ))



(add-hook 'focus-out-hook 'save-all)

;; prefer 115200 baud
(setq serial-speed-history
      (list  "115200";; Given twice because "115200" b/s is the most common speed
             "1200" "2400" "4800" "9600" "14400" "19200"
             "28800" "38400" "57600"   "115200"))

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


;; Stop autocompleting numbers
                                        ;(push (apply-partially #'cl-remove-if
                                        ;                       (lambda (c) (string-match-p "\\`[0-9]+[a-f]+\\'" c)))
                                        ;      company-transformers)
