;;; Commentary - My customizations for macs.
;;; Code

(setq mac-command-key-is-meta t) ; apple = meta
(setq mac-pass-command-to-system nil) ; avoid hiding with M-h

;; Ignore .DS_Store files with helm mode
(add-to-list 'helm-boring-buffer-regexp-list "\\.DS_Store$")

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))
(provide 'mac)
;;; mac.el ends here
