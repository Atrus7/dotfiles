(defconst cf-ide-packages '(lsp-mode cquery flycheck company))

;; IDE-like features generally slow emacs down. Offers a single On-Off switch for these features
;; Enables/Disables the following modes: company, flycheck, and lsp-mode

(define-minor-mode cf/ide-mode
  "Toggle ide mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state. "
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " IDE"
  ;; The minor mode bindings.
  '()
  ;; on end
  ;; (lsp-mode nil) ;; is this suff
  :group 'ide)


(defun cf-ide/init-lsp-mode()
  (use-package lsp-mode
    :defer t
    :config
    )
  )

(defun cf-ide/init-cquery()
  (use-package cquery
    :defer t
    :config
    (setq cquery-executable "/usr/sbin/cquery"

          ;; Log file
          cquery-extra-args '("--log-file=/tmp/cquery.log")

          ;; Cache directory, both relative and absolute paths are supported
          cquery-cache-dir "~/tmp/.cquery_cached_index")
    ))

(defun cf-ide/post-init-company()
  (use-package company
    :defer t
    :config
    (setq company-idle-delay nil) ;; require tab to compelete
    (add-hook 'c++-mode-hook
              (lambda ()
                (setq company-clang-arguments '("-std=c++11")
                      flycheck-clang-language-standard "c++11")))
    ))

(defun cf-ide/post-init-flycheck()
  (use-package flycheck
    :defer t
    :config
    (setq syntax-checking-enable-by-default nil)
    (flycheck-mode nil)
    ))
