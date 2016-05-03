;;; Commentary - My customizations for Emacs.
;;; Code
;; Open files


(defun cf/configure-os-darwin ()

  (defun mac-open-current-file ()
    (interactive)
    (shell-command (concat "open " (buffer-file-name))))

  (setq mac-command-key-is-meta t ; apple = meta
        mac-pass-command-to-system nil ; avoid hiding with M-h
        )
  ;; Ignore .DS_Store files with helm mode
  (add-to-list 'helm-boring-buffer-regexp-list "\\.DS_Store$")

  (set-default-font "Monaco 10"))

(el-init-provide)
;;; mac.el ends here
