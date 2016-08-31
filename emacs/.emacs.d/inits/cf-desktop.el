;;; Desktop work
;; Be non-interactive while starting a daemon.

;; HOW do we do this without ruining buffers?
(when (daemonp)
  (defadvice my-desktop-restore-file-buffer-advice
    (around my-desktop-restore-file-buffer)
    "Be non-interactive while starting a daemon."
    (let ((noninteractive t))
      ad-do-it))
  (ad-activate 'my-desktop-restore-file-buffer-advice))

(desktop-save-mode 1) ; remember what I had open

(setq desktop-base-file-name ".emacs.desktop"
      desktop-load-locked-desktop t
      desktop-files-not-to-save   "^$" ;reload tramp paths)

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

;;; cf-desktop.el ends here
(el-init-provide)
