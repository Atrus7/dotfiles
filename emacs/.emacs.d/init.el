;;; init.el --- Chris Initialization File
;;
;; Copyright (c) 2015 Christopher Findeisen
;;
;; Author: Christopher Findeisen <cfindeisen7@gmail.com>
;;
;;
;;; License: GPLv3

;;; Commentary: This file installs and loads the required packages,
;;; and then it uses el-init to load the rest



(setq required-packages
  '(
    el-init
    cl-lib
    slime
    magit
    evil
    evil-leader
    helm
    company
    evil-surround
    key-chord
    solarized-theme
    gruvbox-theme
    crosshairs
    theme-changer
    jedi
    diminish
    rainbow-delimiters
    projectile
    deferred
    midnight
    guide-key
    company-jedi
    linum-relative
    flycheck
    yasnippet
  ))

;;; Package management
(require 'cl-lib) ;common lisp
(require 'package) ; MELPA

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package required-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;; Now my Emacs Configurations
(require 'el-init)
(el-init-load "~/.emacs.d/inits"
              :subdirectories '("." "inits" "lang"))

;(add-to-list 'load-path "~/.emacs.d/my-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'cf)
(require 'picky-linum)
;(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg")
;(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg/emacs-async")
;(add-to-list 'load-path "~/.emacs.d/config")
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg/helm")



;(global-auto-revert-mode)

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c =") 'auto-fill-mode)


;;; Nice but more opinionated Settings. Make it great!
;; Relative line numbering
(require 'linum-relative)
(setq linum-relative-current-symbol "")
;; Great parens :)
(require 'rainbow-delimiters); byte-compile rainbow delimiters for speed
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(electric-pair-mode 1)
(electric-indent-mode 1)

;; Guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-c" "C-x" "C-x r" "C-c p"))
;(setq guide-key/highlight-command-regexp
      ;'("rectangle"
        ;("C" . "hot-pink")))
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("helm" . "hot pink")))
(guide-key-mode 1)

;; Crosshairs for finding cursor
(require 'crosshairs)
(toggle-crosshairs-when-idle 1)
(setq col-highlight-vline-face-flag  t
      col-highlight-face             hl-line-face)
(global-hl-line-mode 1)
;;TODO: Get horizontal line to stay
;;(set-face-attribute 'default t :font  FONT )
;(set-face-attribute 'default nil
;                     :height
;                    (5))


;; Mode line should look nicer...
(require 'diminish)

(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
                                        ;(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

;(eval-after-load "projectile"
 ;'(diminish 'projectile-mode (format " P:|%s|" (projectile-project-name)))
;)

;;; Theme -- I like colors.
(setq calendar-location-name "Austin, TX")
(setq calendar-latitude [30 18 north] )
(setq calendar-longitude [97 44 west] )
(require 'theme-changer) ; Let it be stark when it's dark, and light when it's bright
(change-theme 'gruvbox 'solarized-light)
(setq solarized-distinct-fringe-background nil)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-more-italic)

;; Spell checking :)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Remove whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'midnight)
(midnight-delay-set 'midnight-delay "3:30am")

;;;Projectile --- Duck!
(projectile-global-mode 1)
(setq projectile-enable-caching t)
;;; Evil -- We've joined the dark side.
(require 'evil)
;;Evilescape??

(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-normal-state-map (kbd "C-j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-previous-visual-line)

; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") (lambda() (interactive ) (previous-line) (newline-below-point) ))

(require 'evil-leader)

;; creates a newline without breaking the current line
(defun newline-below-point ()
  "1. Move to end of line
   2. insert newline with indentation"
  (interactive)
  (let((oldpos(point)))
    (end-of-line)
    (newline-and-indent)))
;;; Space -- Out of this world
(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")
(evil-mode 1) ;; this line must be after we set the leader
(evil-leader/set-key
    ;; Super important one-key spacecuts
  "d" 'dired
  "f" 'helm-find-files
  ;; Buffer stuff
  "bb" 'helm-buffers-list
  "bs" 'switch-to-scratch-and-back

  ;"rb" 'revert-buffer

  ;; Misc
  "gs" 'magit-status
  "gc" 'count-words

  ;; Evaluation
  "eb" 'eval-buffer
  "el" 'eval-expression
  "er" 'eval-region

  ;; Window management
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wd" 'evil-window-delete
  "wj" (lambda() (interactive) ( evil-window-decrease-height 5 ))
  "wk" (lambda() (interactive) ( evil-window-increase-height 5 ))
  "wh" (lambda() (interactive) ( evil-window-decrease-width 5 ))
  "wl" (lambda() (interactive) ( evil-window-increase-width 5 ))
  "wu" 'winner-undo
  "wr" 'winner-redo

  ;; Frame managmenent
  "Fn" 'make-frame-command
  "Fd" 'delete-frame
  "Fo" 'other-frame

  "Fs" 'window-configuration-to-register ; Save Frame with buffer and layouts( not to disk...yet )
  "Fl" 'jump-to-register  ; load Frame with buffers and layouts

  ;; Relative Line numbers
  "r" (lambda() (interactive) (linum-relative-toggle))

  ;; Theme stuff
  "tl" (lambda() (interactive) (load-theme 'solarized-light 'NO-CONFIRM))
  "td" (lambda() (interactive) (load-theme 'gruvbox 'NO-CONFIRM))
                                        ;"Fd" ;delete frame
                                        ;"Fo" '
  )


;; Tabs are evil
(setq-default indent-tabs-mode nil)


(require 'evil-surround)
(global-evil-surround-mode 1)


(setq inferior-lisp-program "clisp")

(require 'key-chord)
(key-chord-mode 1)
;;(setq backup-directory-alist '(("." . "~/.emacs-backups")))? Maybe later
;;stop littering with save files, put them here
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Window movement mappings. Global like the Wolf
(dolist (key '("\M-h" "\M-j" "\M-k" "\M-l"))
  (global-unset-key key))


(define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)

(define-key evil-motion-state-map (kbd "M-e") 'eval-buffer)
(define-key evil-motion-state-map (kbd "M-;") 'append-semicolon)


;;; Completion -- Welcome to the firm.
(company-mode 1)
(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
      (indent-according-to-mode)))

(define-key evil-insert-state-map "\t" 'indent-or-complete)

(define-key company-active-map (kbd "C-j") #'company-select-next)
(define-key company-active-map (kbd "C-k") #'company-select-previous)
(define-key company-active-map (kbd "C-l") #'company-complete)
(add-hook 'after-init-hook 'global-company-mode) ; All the buffers

; append a semicolon
(defun append-semicolon()
  "Puts a semicolon at the end of the current line"
  (interactive)
  (end-of-line)
  (insert ";")
  )

(defvar cf/toggle-scratch-buffer nil)
(defun switch-to-scratch-and-back ()
    "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
    (interactive)
    (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
        (if (equal (current-buffer) scratch-buffer-name)
            (switch-to-buffer cf/toggle-scratch-buffer)
          (setq cf/toggle-scratch-buffer (current-buffer))
          (switch-to-buffer scratch-buffer-name )
)))



;; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)

;;; Magit - The git genie
(require 'magit)
(require 'evil-magit)
(setq magit-auto-revert-mode nil)
;(after-load 'magit (add-hook 'magit-mode-hook (lambda () (local-unset-key [(kbd "M-h")]))))
                                        ;(require 'multiple-cursors)
                                        ;(global-set-key (kbd "M-n") 'mc/mark-next-word-like-this)
                                        ;(kbd "M-N") 'mc/unmark-next-like-this
                                        ;(kbd "M-p") 'mc/mark-previous-like-this
                                        ;(kbd "M-P") 'mc/unmark-previous-like-this
                                        ;(kbd "M-c") 'mc/edit-lines)
                                        ;(autopair-global-mode 1)-

;; Start to insert mode when editing commit messages
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

;(defun evil-magit-rebellion-quit-keymode ()
  ;(interactive)
  ;(magit-key-mode-command nil))
;
;(evil-set-initial-state 'magit-mode 'motion)(evil-set-initial-state 'magit-commit-mode 'motion)
;(define-key magit-mode-map (kbd "M-h") nil)
;(evil-define-key 'motion magit-commit-mode-map
  ;"\C-c\C-b" 'magit-show-commit-backward
  ;"\C-c\C-f" 'magit-show-commit-forward)
;
;(evil-set-initial-state 'magit-status-mode 'motion)
;(evil-define-key 'motion magit-status-mode-map
  ;"\C-f" 'evil-scroll-page-down
  ;"\C-b" 'evil-scroll-page-up
  ;"\t" 'magit-toggle-section
  ;"." 'magit-mark-item
  ;"=" 'magit-diff-with-mark
  ;"C" 'magit-add-log
  ;"I" 'magit-ignore-item-locally
  ;"S" 'magit-stage-all
  ;"U" 'magit-unstage-all
  ;"X" 'magit-reset-working-tree
  ;"d" 'magit-discard-item
  ;"i" 'magit-ignore-item
  ;"s" 'magit-stage-item
  ;"u" 'magit-unstage-item
  ;"z" 'magit-key-mode-popup-stashing)
;
;(evil-set-initial-state 'magit-log-mode 'motion)
;(evil-define-key 'motion magit-log-mode-map
  ;"." 'magit-mark-item
  ;"=" 'magit-diff-with-mark
  ;"e" 'magit-log-show-more-entries)
;
;(evil-set-initial-state 'magit-wassup-mode 'motion)
;(evil-define-key 'motion magit-wazzup-mode-map
  ;"." 'magit-mark-item
  ;"=" 'magit-diff-with-mark
  ;"i" 'magit-ignore-item)
;
;(evil-set-initial-state 'magit-branch-manager-mode 'motion)
;(evil-define-key 'motion magit-branch-manager-mode-map
  ;"a" 'magit-add-remote
  ;"c" 'magit-rename-item
  ;"d" 'magit-discard-item
  ;"o" 'magit-create-branch
  ;"v" 'magit-show-branches
  ;"T" 'magit-change-what-branch-tracks)
;
;;; "1" 'magit-show-level-1
;;; "2" 'magit-show-level-2
;;; "3" 'magit-show-level-3
;;; "4" 'magit-show-level-4
;
;(evil-set-initial-state 'magit-mode 'motion)
;(evil-define-key 'motion magit-mode-map
  ;"\M-1" 'magit-show-level-1-all
  ;"\M-2" 'magit-show-level-2-all
  ;"\M-3" 'magit-show-level-3-all
  ;"\M-4" 'magit-show-level-4-all
  ;"\M-H" 'magit-show-only-files-all
  ;"\M-o" 'magit-show-only-files
  ;"\M-S" 'magit-show-level-4-all
  ;"\M-h" 'magit-show-only-files
  ;"\M-s" 'magit-show-level-4
  ;"!" 'magit-key-mode-popup-running
  ;"$" 'magit-process
  ;"+" 'magit-diff-larger-hunks
  ;"-" 'magit-diff-smaller-hunks
  ;"=" 'magit-diff-default-hunks
  ;"/" 'evil-search-forward
  ;":" 'evil-ex
  ;";" 'magit-git-command
  ;"?" 'evil-search-backward
  ;"<" 'magit-key-mode-popup-stashing
  ;"A" 'magit-cherry-pick-item
  ;"B" 'magit-key-mode-popup-bisecting
                                        ;;C  commit add log
  ;"D" 'magit-revert-item
  ;"E" 'magit-ediff
  ;"F" 'magit-key-mode-popup-pulling
  ;"G" 'evil-goto-line
  ;"H" 'magit-rebase-step
                                        ;;I  ignore item locally
  ;"J" 'magit-key-mode-popup-apply-mailbox
  ;"K" 'magit-key-mode-popup-dispatch
  ;"L" 'magit-add-change-log-entry
  ;"M" 'magit-key-mode-popup-remoting
  ;"N" 'evil-search-previous
                                        ;;O  undefined
  ;"P" 'magit-key-mode-popup-pushing
                                        ;;Q  undefined
  ;"R" 'magit-refresh-all
  ;"S" 'magit-stage-all
                                        ;;T  change what branch tracks
  ;"U" 'magit-unstage-all
                                        ;;V  visual line
  ;"W" 'magit-diff-working-tree
  ;"X" 'magit-reset-working-tree
  ;"Y" 'magit-interactive-rebase
  ;"Z" 'magit-key-mode-popup-stashing
  ;"a" 'magit-apply-item
  ;"b" 'magit-key-mode-popup-branching
  ;"c" 'magit-key-mode-popup-committing
                                        ;;d  discard
  ;"e" 'magit-diff
  ;"f" 'magit-key-mode-popup-fetching
  ;"g" 'magit-refresh
  ;"h" 'magit-key-mode-popup-rewriting
  ;"j" 'evil-next-visual-line
  ;"k" 'evil-previous-visual-line
  ;"]]" 'magit-goto-next-section
  ;"[[" 'magit-goto-previous-section
  ;"l" 'magit-key-mode-popup-logging
  ;"m" 'magit-key-mode-popup-merging
  ;"t" 'magit-key-mode-popup-tagging
  ;)
;
;;; (when magit-rigid-key-bindings
;;;   (evil-define-key 'motion magit-mode-map
;;;     "!" 'magit-git-command-topdir
;;;     "B" 'undefined
;;;     "F" 'magit-pull
;;;     "J" 'magit-apply-mailbox
;;;     "M" 'magit-branch-manager
;;;     "P" 'magit-push
;;;     "b" 'magit-checkout
;;;     "c" 'magit-commit
;;;     "f" 'magit-fetch-current
;;;     "h" 'undefined
;;;     "l" 'magit-log
;;;     "m" 'magit-merge
;;;     "o" 'magit-submodule-update
;;;     "t" 'magit-tag
;;;     "z" 'magit-stash))
;
;(provide 'evil-magit-rebellion)
;(eval-after-load 'magit
  ;'(progn
    ;(require 'evil-magit-rebellion)))


(winner-mode 1)
(setq magit-last-seen-setup-instructions "1.4.0")


;;; Flycheck - It's perfect when my fingers aren't.
(add-hook 'after-init-hook #'global-flycheck-mode)
                                        ;(add-hook 'python-mode 'flycheck-mode)
(setq flycheck-flake8-maximum-line-length 119)

;; Helm Config stuff
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(helm-adaptive-mode 1)

(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq helm-display-source-at-screen-top nil)
(setq helm-display-header-line t)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-previous-source)
(define-key helm-map (kbd "C-l") 'helm-next-source)

;;FINALLY GOT SANE HELM Find-File MAPPINGS WOOHOO!
(define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

(setq helm-autoresize-max-height 30
      helm-autoresize-min-height 30
      helm-split-window-in-side-p t
      helm-ff-auto-update-initial-value               t
      helm-grep-default-command                       "ack-grep -Hn --smart-case --no-group %e %p %f"
      helm-org-headings-fontify                       t




)


;;Helm Ignore
(add-hook 'helm-before-initialize-hook
          (lambda ()
            (add-to-list 'helm-boring-buffer-regexp-list "\\.pyc$")
            (add-to-list 'helm-boring-buffer-regexp-list "\\.o$")))
(setq helm-ff-skip-boring-files t)

;;; Yay Snippets
(require 'yasnippet)
(yas-global-mode 1)

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

;;; Mac specific
(when (eq system-type "darwin")
  (require 'mac)
  )

;;;Linux specific
(if (eq system-type "gnu/linux")
    ;;Assuming work comp
    (setq jedi:server-command '( "/home/cfindeisen/.emacs.d/.python-environments/default/bin/jediepcserver" ))
                                        ;(load-library "p4")
                                        ;print "On Linux"
                                        ;(p4-set-p4-executable "/home/cfindeisen/Downloads/p4v-2014.3.1007540/bin/p4v.bin")
  (message "Linux Recognized. Configuring...")
  (cf/configure-os-linux))

;;; Utility functions

;(cf/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
;(cf/move-key evil-motion-state-map evil-normal-state-map " ")

;;;Language specific
;; C
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;;Python
(add-hook 'python-mode 'run-python) ; starts inferior python process
                                        ;(remove-hook 'python-mode-hook 'run-python)

(autoload 'jedi:setup "jedi" nil t)
;;(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(defun my/python-mode-hook()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
                                        ;(eval-after-load "company"
                                        ;'(progn
                                        ;(add-to-list 'company-backends 'company-jedi)))

;;; Comint (SHELL)
(setq comint-prompt-read-only t
      comint-process-echoes nil)

;;; Emacs-Added(Customize vars)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil))
'(custom-safe-themes
  (quote
   ("badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
(provide 'init)
;;; init.el ends here