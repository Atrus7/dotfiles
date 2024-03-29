;; Spacemacs is not opinionated enough or not enough like me.

;; The place to save all my scratch buffers I end up keeping...
(defvar cf/scratch-save-dir "~/tmp")

(add-hook 'git-commit-mode-hook
          (lambda ()
            (setq fill-column 72)))
(setq python-shell-interpreter "python3")

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


(with-eval-after-load 'hl-line+
  (global-hl-line-mode 0)
  (toggle-hl-line-when-idle 1)
  )

;; Dired
(add-hook 'dired-mode-hook 'hl-line-mode)
(setq dired-listing-switches
      "-AFhlv")

(setq magit-use-sticky-arguments "selected")


(defun my-diff-long-lines ()
  "Disable `truncate-lines' in the current buffer."
  (setq-local truncate-lines nil)
  (setq-local magit-diff-refine-hunk t))

(add-hook 'magit-diff-mode-hook #'my-diff-long-lines)

;; files that require a password are annoying if they hang around
(with-eval-after-load
    "recentf"
  (add-to-list 'recentf-exclude ".*\.gpg")
  (add-to-list 'recentf-exclude "/sudo:.*"))

(setq shell-default-shell 'vterm)
(with-eval-after-load "vterm"
  (setq vterm-shell "/bin/zsh"))

(setq sh-make-vars-local nil          ; Don't edit any shell files except my own
      create-lockfiles nil
      vc-follow-symlinks t
      initial-scratch-message "* Scratch Buffer\n"
      doc-view-continuous t
      auto-revert-remote-files nil ;; otherwise this takes a long time...
      )

;; May want to remove this if not handling big repos
;; Make magit faster for giant buffers
;; https://magit.vc/manual/magit/Performance.html
(setq vc-handled-backends nil
      magit-revision-insert-related-refs nil)
;; (setq vc-handled-backends '(RCS CVS SVN SRC Git Hg)) ;; only use likely backends

(setq
 ivy-fixed-height-minibuffer t
 ivy-height 20
 ivy-initial-inputs-alist nil
 ivy-count-format "%-4d ")

;; to make visual copy work inside of emacs in tmux
(xterm-mouse-mode -1)

;; Better mouse scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

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
(setq undo-tree-auto-save-history nil)

(add-hook 'before-save-hook 'cf/delete-trailing-whitespace)

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

(defun spaceline-config ()
  "Relic from when I used Spaceline"
  (setq-default
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme '(spacemacs :separator bar :separator-scale 1.0))
  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-mode-line-org-clock-on)

  (spaceline-define-segment narrow
    "Display Narrowed when buffer is narrowed."
    (when (buffer-narrowed-p)
      "Narrowed"))

  (spaceline-spacemacs-theme 'narrow)
  )


;; Evil's own search impl isn't as performant as isearch is. Annoying when working in large buffers
(setq evil-search-module 'isearch)
