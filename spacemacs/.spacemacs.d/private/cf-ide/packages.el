(defconst cf-ide-packages '(lsp-mode cquery flycheck company semantic))

;; IDE-like features generally slow emacs down. Offers a single On-Off switch for these features
;; Enables/Disables the following modes: company, flycheck, and lsp-mode

(define-minor-mode cf/ide-mode
  "Toggle ide mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  "
  ;; The initial value.
  nil " IDE" '() :group 'ide)

(spacemacs|add-toggle cf/ide-mode
  :status cf/ide-mode
  :on (progn (cf/ide-mode 1)
             (enable-ide-features))
  :off (progn (cf/ide-mode -1)
              (disable-ide-features))
  )


(defun cf-ide/init-lsp-mode()
  (use-package lsp-mode
    :defer t
    :config
    (setq cquery-project-roots '())
    ;; (require 'company-lsp)
    ))

(defun cf-ide/init-cquery()
  (use-package cquery
    :commands lsp
    :defer t
    :config
    (require 'projectile)
    (setq cquery-executable (locate-file "cquery" exec-path)

          ;; Log file
          cquery-extra-args '("--log-file=/tmp/cquery.log")

          ;; Cache directory, both relative and absolute paths are supported
          cquery-cache-dir "~/tmp/.cquery_cached_index"))
  )

(defun cf-ide/post-init-semantic()
  (use-package semantic
    :config
    (semantic-mode -1) ;; off by default
    (add-hook 'semantic-mode-hook 'maybe-semantic-mode)
    )
  )

(defun cf-ide/post-init-company()
  (use-package company
    :defer t
    :config
    (evil-define-key 'insert 'global (kbd "C-l") 'company-complete)

  ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;;; - https://emacs.stackexchange.com/a/13290/12534
  ;;; - http://stackoverflow.com/a/22863701/3538165
  ;;;
  ;;; See also:
  ;;; - https://emacs.stackexchange.com/a/24800/12534
  ;;; - https://emacs.stackexchange.com/q/27459/12534

  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)
  (setq company-tooltip-limit 5)
  (setq company-idle-delay 0)

  ;; Company appears to override the above keymap based on company-auto-complete-chars.
  ;; Turning it off ensures we have full control.
  (setq company-auto-complete-chars nil)

    (add-hook 'c++-mode-hook
              (lambda ()
                (setq company-clang-arguments '("-std=c++11")
                      flycheck-clang-language-standard "c++11")))
    ))

;; (defun cf-ide/init-company-lsp()
;;   (use-package company-lsp
;;     :defer t
;;     :commands company-lsp
;;     :config
;;     (setq company-lsp-cache-candidates 'auto
;;           company-lsp-async t
;;           company-lsp-enable-recompletion t)
;;     ))

(defun cf-ide/post-init-flycheck()
  (use-package flycheck
    :defer t
    :config
    (setq syntax-checking-enable-by-default nil)
    ))
