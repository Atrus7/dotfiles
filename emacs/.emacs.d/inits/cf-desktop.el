;;; Desktop work
(desktop-save-mode 1) ; remember what I had open
(setq desktop-base-file-name ".emacs.desktop")

;;; desktop-override-stale-locks
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;(defadvice desktop-restore-file-buffer
  ;(around my-desktop-restore-file-buffer-advice)
  ;"Be non-interactive while starting a daemon."
  ;(if (and (daemonp)
           ;(not server-process))
      ;(let ((noninteractive t))
        ;ad-do-it)
    ;ad-do-it))
;(ad-activate 'desktop-restore-file-buffer)
;;; cf-desktop.el ends here
(el-init-provide)
