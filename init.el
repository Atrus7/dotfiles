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
 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
 
(when (not package-archive-contents)
  (package-refresh-contents))
 
(defvar my-packages '(evil company))
 
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(require 'evil)
(evil-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)

(show-paren-mode 1)
(global-linum-mode 1) ; display line numbers

;; Use company-mode in all buffers (more completion)
(add-hook 'after-init-hook 'global-company-mode)

;;remember to byte-compile rainbow delimiters for speed whenever I am moving configs
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

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


;;remember what I had open
(desktop-save-mode 1)

;;Evil tabs
(setq-default indent-tabs-mode nil)
