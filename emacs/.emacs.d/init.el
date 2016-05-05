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
;; notes
;; tag usage...
;; map 0 to last kbd macro
;; narrow-to-region
;; dired wildcards
;; increment register
;; gdb work steps through code


;;; Code:

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
    evil-magit
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
    org-ref
    vimrc-mode
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
(el-init-load "~/.emacs.d/inits" :subdirectories '("." "inits" "lang"))

;;; Emacs-Added(Customize vars)
;(custom-set-variables
 ;;; custom-set-variables was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 ;'(ansi-color-names-vector
   ;["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 ;'(custom-safe-themes
   ;(quote
    ;("d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" default)))
 ;'(custom-safe-themes
   ;(quote
    ;("badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default)))
 (provide 'init)
;;; init.el ends here
