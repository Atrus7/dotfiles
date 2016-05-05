;;; Commentary: This will handle all of the other settings that we need to configure beyond sanity purposes.
;;; Sort of a miscellaneous area
(require 'key-chord)
(require 'midnight)

(winner-mode 1) ; get out of jams with win-undo

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(key-chord-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs-backups")));? Maybe later
;;stop littering with save files, put them here
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(add-hook 'focus-out-hook 'save-buffer) ; auto-save
                                        ;(defun auto-save-programs)

;; Great parens :)
(electric-pair-mode 1)
;; Tabs are evil
(setq-default indent-tabs-mode nil)
(electric-indent-mode 1)

;; Can we get this to work?
;; (global-auto-revert-mode)


;; WHY IS THIS HERE??
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Remove whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(midnight-delay-set 'midnight-delay "3:30am")



(el-init-provide)
