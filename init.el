;;; init.el --- Chris Initialization File
;;
;; Copyright (c) 2015-6-4 Christopher Findeisen
;;
;; Author: Christopher Findeisen <cfindeisen7@gmail.com>
;;
;;
;;; License: GPLv3

;;; Detect OS


;;; Package management
(require 'cl-lib) ;common lisp


(require 'package) ; MELPA

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;;; Auto-install
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(
    magit
    evil
    helm
    company
    evil-surround
    key-chord
    solarized-theme

  ) "a list of packages to ensure are installed at launch.")
; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg")
(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg/emacs-async")
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/chris-shmorgishborg/helm")

;;; General sane settings

(global-auto-revert-mode t)


(show-paren-mode 1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1)
(tool-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq visible-bell 1) ; visual rather than auditory
(desktop-save-mode 1) ; remember what I had open
(fset 'yes-or-no-p 'y-or-n-p) ; Changes all yes/no questions to y/n type

;;; Nice but more opinionated Settings. Make it great!

;; Great parens :)
(require 'rainbow-delimiters); byte-compile rainbow delimiters for speed
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(electric-pair-mode 1)
(electric-indent-mode 1)

;;; Theme -- I like colors.
(load-theme 'solarized-dark t)
(setq solarized-distinct-fringe-background t)

;;; Evil -- We've joined the dark side.
(require 'evil)
(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

(evil-leader/set-key
  "e" 'eval-buffer
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  "gs" 'magit-status
  ;;window management
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wd" 'evil-window-delete
  "wu" 'winner-undo
  "wr" 'winner-redo
)


;; Tabs are evil
(setq-default indent-tabs-mode nil)

;; Use company-mode in all buffers (more completion)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-company-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)


(require 'key-chord)
(key-chord-mode 1)
;;(setq backup-directory-alist '(("." . "~/.emacs-backups")))? Maybe later
;;stop littering with save files, put them here
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;Window mappings
(define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "M-e") 'eval-buffer)

(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


; (require 'autopair)
; (autopair-global-mode 1)



;;; Magit - The git genie
(require 'magit)
(setq magit-auto-revert-mode nil)


;; Start to insert mode when editing commit messages
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

(defun evil-magit-rebellion-quit-keymode ()
  (interactive)
  (magit-key-mode-command nil))

(evil-set-initial-state 'magit-mode 'motion)(evil-set-initial-state 'magit-commit-mode 'motion)
(evil-define-key 'motion magit-commit-mode-map
  "\C-c\C-b" 'magit-show-commit-backward
  "\C-c\C-f" 'magit-show-commit-forward)

(evil-set-initial-state 'magit-status-mode 'motion)
(evil-define-key 'motion magit-status-mode-map
  "\C-f" 'evil-scroll-page-down
  "\C-b" 'evil-scroll-page-up
  "\t" 'magit-toggle-section
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "C" 'magit-add-log
  "I" 'magit-ignore-item-locally
  "S" 'magit-stage-all
  "U" 'magit-unstage-all
  "X" 'magit-reset-working-tree
  "d" 'magit-discard-item
  "i" 'magit-ignore-item
  "s" 'magit-stage-item
  "u" 'magit-unstage-item
  "z" 'magit-key-mode-popup-stashing)

(evil-set-initial-state 'magit-log-mode 'motion)
(evil-define-key 'motion magit-log-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "e" 'magit-log-show-more-entries)

(evil-set-initial-state 'magit-wassup-mode 'motion)
(evil-define-key 'motion magit-wazzup-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "i" 'magit-ignore-item)

(evil-set-initial-state 'magit-branch-manager-mode 'motion)
(evil-define-key 'motion magit-branch-manager-mode-map
  "a" 'magit-add-remote
  "c" 'magit-rename-item
  "d" 'magit-discard-item
  "o" 'magit-create-branch
  "v" 'magit-show-branches
  "T" 'magit-change-what-branch-tracks)

;; "1" 'magit-show-level-1
;; "2" 'magit-show-level-2
;; "3" 'magit-show-level-3
;; "4" 'magit-show-level-4

(evil-set-initial-state 'magit-mode 'motion)
(evil-define-key 'motion magit-mode-map
  "\M-1" 'magit-show-level-1-all
  "\M-2" 'magit-show-level-2-all
  "\M-3" 'magit-show-level-3-all
  "\M-4" 'magit-show-level-4-all
  "\M-H" 'magit-show-only-files-all
  "\M-S" 'magit-show-level-4-all
  "\M-h" 'magit-show-only-files
  "\M-s" 'magit-show-level-4
  "!" 'magit-key-mode-popup-running
  "$" 'magit-process
  "+" 'magit-diff-larger-hunks
  "-" 'magit-diff-smaller-hunks
  "=" 'magit-diff-default-hunks
  "/" 'evil-search-forward
  ":" 'evil-ex
  ";" 'magit-git-command
  "?" 'evil-search-backward
  "<" 'magit-key-mode-popup-stashing
  "A" 'magit-cherry-pick-item
  "B" 'magit-key-mode-popup-bisecting
  ;C  commit add log
  "D" 'magit-revert-item
  "E" 'magit-ediff
  "F" 'magit-key-mode-popup-pulling
  "G" 'evil-goto-line
  "H" 'magit-rebase-step
  ;I  ignore item locally
  "J" 'magit-key-mode-popup-apply-mailbox
  "K" 'magit-key-mode-popup-dispatch
  "L" 'magit-add-change-log-entry
  "M" 'magit-key-mode-popup-remoting
  "N" 'evil-search-previous
  ;O  undefined
  "P" 'magit-key-mode-popup-pushing
  ;Q  undefined
  "R" 'magit-refresh-all
  "S" 'magit-stage-all
  ;T  change what branch tracks
  "U" 'magit-unstage-all
  ;V  visual line
  "W" 'magit-diff-working-tree
  "X" 'magit-reset-working-tree
  "Y" 'magit-interactive-rebase
  "Z" 'magit-key-mode-popup-stashing
  "a" 'magit-apply-item
  "b" 'magit-key-mode-popup-branching
  "c" 'magit-key-mode-popup-committing
  ;d  discard
  "e" 'magit-diff
  "f" 'magit-key-mode-popup-fetching
  "g?" 'magit-describe-item
  "g$" 'evil-end-of-visual-line
  "g0" 'evil-beginning-of-visual-line
  "gE" 'evil-backward-WORD-end
  "g^" 'evil-first-non-blank-of-visual-line
  "g_" 'evil-last-non-blank
  "gd" 'evil-goto-definition
  "ge" 'evil-backward-word-end
  "gg" 'evil-goto-first-line
  "gj" 'evil-next-visual-line
  "gk" 'evil-previous-visual-line
  "gm" 'evil-middle-of-visual-line
  "h" 'magit-key-mode-popup-rewriting
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section
  "l" 'magit-key-mode-popup-logging
  "m" 'magit-key-mode-popup-merging
  "t" 'magit-key-mode-popup-tagging
  )

(when magit-rigid-key-bindings
  (evil-define-key 'motion magit-mode-map
    "!" 'magit-git-command-topdir
    "B" 'undefined
    "F" 'magit-pull
    "J" 'magit-apply-mailbox
    "M" 'magit-branch-manager
    "P" 'magit-push
    "b" 'magit-checkout
    "c" 'magit-commit
    "f" 'magit-fetch-current
    "h" 'undefined
    "l" 'magit-log
    "m" 'magit-merge
    "o" 'magit-submodule-update
    "t" 'magit-tag
    "z" 'magit-stash))

(provide 'evil-magit-rebellion)
(eval-after-load 'magit
  '(progn
     (require 'evil-magit-rebellion)))





;; Remember what I had open when I quit
(desktop-save-mode 1)
(winner-mode 1)
 (setq magit-last-seen-setup-instructions "1.4.0")


 ;; Helm Config stuff
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-display-source-at-screen-top nil)
(setq helm-display-header-line t)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-h") 'helm-previous-source)
(define-key helm-map (kbd "C-l") 'helm-next-source)

;;FINALLY GOT SANE HELM Find-File MAPPINGS WOOHOO!
(define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
(setq helm-split-window-in-side-p t)

;;; Mac specific
(if (eq system-type "darwin")
    (setq mac-command-key-is-meta t);apple = meta
    (setq mac-pass-command-to-system nil);avoid hiding with M-h

)

;;;Linux specific
(if (eq system-type 'gnu/linux)
    message system-type
    ;(load-library "p4")
    ;(p4-set-p4-executable "/home/cfindeisen/Downloads/p4v-2014.3.1007540/bin/p4v.bin")
)

;;; Utility functions

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;;;Language specific
;; C
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
;;Python


;;;Emacs-Added(Customize vars)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
 '(magit-diff-use-overlays nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
