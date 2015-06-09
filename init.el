;;; init.el --- Chris Initialization File
;;
;; Copyright (c) 2015-6-4 Christopher Findeisen
;;
;; Author: Christopher Findeisen <cfindeisen7@gmail.com>
;;
;;
;;; License: GPLv3
(require 'cl-lib) ;common lisp

; MELPA
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil company))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'evil)
(evil-mode 1)

(add-to-list 'load-path "~/.emacs.d/chris-schmorgishborg/helm")
(add-to-list 'load-path "~/.emacs.d/chris-schmorgishborg/emacs-async")

(require 'helm-config)
(helm-mode 1)

;(global-set-key (kbd "SPC") SPC)
;(global-set-key (kbd "SPC f") 'helm-buffers-list)
;(global-set-key (kbd "SPC b") 'helm-buffers-list)


(add-to-list 'load-path "~/.emacs.d/config")


(global-auto-revert-mode t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)

(show-paren-mode 1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Tabs are evil
;;(setq-default indent-tabs-mode nil)

;; Use company-mode in all buffers (more completion)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-company-mode)

;;remember to byte-compile rainbow delimiters for speed whenever I am moving configs
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'magit)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

;;stop littering with save files, put them here
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "M-e") 'eval-buffer)


;;remember what I had open
(desktop-save-mode 1)

;;Evil tabs
(setq-default indent-tabs-mode nil)
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

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'eval-buffer
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  "gs" 'magit-status
  )

;; Remember what I had open when I quit
(desktop-save-mode 1)
(winner-mode 1)
