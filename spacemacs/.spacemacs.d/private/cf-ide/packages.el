(defconst cf-ide-packages '(lsp-mode cquery flycheck company company-lsp semantic))

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
    :defer t
    :config
    (require 'projectile)
    (setq cquery-executable (locate-file "cquery" exec-path)

          ;; Log file
          cquery-extra-args '("--log-file=/tmp/cquery.log")

          ;; Cache directory, both relative and absolute paths are supported
          cquery-cache-dir "~/tmp/.cquery_cached_index"))

  ;; TODO recache upon what circumstances??
  ;; ninja -C out/default/ -t compdb cxx cc > compile_commands.json
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

    (add-hook 'c++-mode-hook
              (lambda ()
                (setq company-clang-arguments '("-std=c++11")
                      flycheck-clang-language-standard "c++11")))
    ))

(defun cf-ide/init-company-lsp()
  (use-package company-lsp
    :defer t
    :config
    (setq company-lsp-cache-candidates 'auto
          company-lsp-async t
          company-lsp-enable-recompletion t)
    ))

(defun cf-ide/post-init-flycheck()
  (use-package flycheck
    :defer t
    :config
    (setq syntax-checking-enable-by-default nil)
    ))
