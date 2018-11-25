;; Spacemacs is not opinionated enough or not enough like me.

;; The place to save all my scratch buffers I end up keeping...
(defvar cf/scratch-save-dir "~/tmp")

(add-hook 'git-commit-mode-hook
          (lambda ()
            (setq fill-column 72)))

;;; It's some work keeping TRAMP out of trouble....
(with-eval-after-load 'tramp
  ;; (setq tramp-ssh-controlmaster-options nil)
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))

  (setq
   shell-file-name "/bin/bash"
   shell-default-term-shell  "/bin/bash"
   tramp-default-method "scp" ;; scp is faster than ssh....
   confirm-kill-emacs (if (display-graphic-p)
                          nil
                        'y-or-n-p ;; avoid lovely Emergency Escape "feature"
                          )

   ;; tramp-ssh-controlmaster-options
   ;; (concat
   ;;  "-o ControlPath=/tmp/.ssh/ssh-ControlPath-%%r@%%h:%%p "
   ;;  "-o ControlMaster=auto -o ControlPersist=yes")
   )

  ;; dired-use-ls-dired
  )
;; (setq tramp-verbose 10)


(setq sh-make-vars-local nil ; Don't edit any shell files except my own
      create-lockfiles nil
      vc-follow-symlinks t
      vc-handled-backends '(RCS CVS SVN SRC Git Hg) ;; only use likely backends
      initial-scratch-message "* Scratch Buffer\n"
      doc-view-continuous t
      auto-revert-remote-files nil ;; otherwise this takes a long time...
      )
(setq
 ivy-fixed-height-minibuffer t
 ivy-height 20
 ivy-initial-inputs-alist nil
 ivy-count-format "%-4d "
 )

(add-hook 'focus-out-hook 'save-all)


(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


(add-hook 'focus-out-hook 'save-all)

;; prefer 115200 baud
(setq serial-speed-history
      (list  "115200";; Given twice because "115200" b/s is the most common speed
             "1200" "2400" "4800" "9600" "14400" "19200"
             "28800" "38400" "57600"   "115200"))

(setq whitespace-style '(face spaces tabs space-mark tab-mark))
(add-hook 'makefile-mode-hook
          (lambda () (whitespace-mode)))

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
