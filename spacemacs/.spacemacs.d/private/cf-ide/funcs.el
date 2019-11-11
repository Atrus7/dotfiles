(defun enable-ide-features ()
  (interactive)
  (require 'cquery)
  (message "IDE features on.")

  (setq company-idle-delay 0.2
        syntax-checking-enable-by-default t
        lsp-highlight-symbol-at-point nil
        lsp-enable-codeaction nil
        lsp-enable-eldoc nil
        lsp-enable-completion-at-point nil
        )

  (cquery-enable-optional) ;; launches lsp as well
  (flycheck-mode 1)
  (semantic-mode 1)
  )

(defun disable-ide-features ()
  (interactive)
  (message "IDE features off.")
  (flycheck-mode -1)
  (lsp-mode -1)
  (semantic-mode -1)
  (eldoc-mode -1)

  (setq syntax-checking-enable-by-default nil
        company-idle-delay nil
        lsp-highlight-symbol-at-point nil
        lsp-enable-codeaction nil
        lsp-enable-eldoc nil
        lsp-enable-completion-at-point nil
        company-backends (remove 'company-lsp company-backends)
        )

  ;; TODO: just reverting the buffer seems like the best way thus far to get back to a clean state..
  ;; Otherwise lsp still litters buffer with font issues...
  (if (cquery--is-cquery-buffer)
      (reload-buffer))
  )

(defun kill-lsp ()
  (interactive)
  (lsp--shutdown-cur-workspace))

(defun cquery-enable-optional ()
  (interactive)
  ;; (if (null (cquery-project-roots-matcher))
  (if (null (cquery--is-cquery-buffer))
      (let ((root (projectile-root-top-down-recurring buffer-file-name '("compile_commands.json"  ".cquery"))))
        (if (null root)
            (message "No cquery here...")
          (progn
                 (message root)
                 (add-to-list 'cquery-project-roots root)
                 (lsp)))
        )
    )

  (if (cquery--is-cquery-buffer)
      (cl-pushnew 'company-lsp company-backends)
    )
  )

(defun maybe-semantic-mode ()
  (remove-hook 'semantic-mode-hook 'maybe-semantic-mode) ;; prevent recurrence
  (if (and (not cf/ide-mode) semantic-mode)
      (progn
        (semantic-mode -1)
        (mapcar (lambda (hook) (remove-hook hook 'semantic-mode))
                '(c-mode-hook c++-mode-hook emacs-lisp-mode-hook python-mode-hook))
        )
    )
  (add-hook 'semantic-mode-hook 'maybe-semantic-mode)
  )
