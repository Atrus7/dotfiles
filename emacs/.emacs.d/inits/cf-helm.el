;;; The Original Helm.sh (minimal configuration)


;(setq package-load-list '((helm-core t) (helm t) (async t)))
;(package-initialize)
;(add-to-list 'load-path (file-name-directory (file-truename "$0")))
;(setq default-frame-alist '((vertical-scroll-bars . nil)
                            ;(tool-bar-lines . 0)
                            ;(menu-bar-lines . 0)
                            ;(fullscreen . nil)))
;(blink-cursor-mode -1)
;(require 'helm-config)
;(helm-mode 1)
;(define-key global-map [remap find-file] 'helm-find-files)
;(define-key global-map [remap occur] 'helm-occur)
;(define-key global-map [remap list-buffers] 'helm-buffers-list)
;(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;(global-set-key (kbd "M-x") 'helm-M-x)
;(unless (boundp 'completion-in-region-function)
  ;(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
                                        ;(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; Helm Config stuff
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(helm-adaptive-mode 1)

(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)

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

(el-init-provide)
