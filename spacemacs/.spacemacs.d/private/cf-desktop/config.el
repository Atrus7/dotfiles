;;; Desktop work
;; Be non-interactive while starting a daemon.
;; this needs to be early in the init, otherwise it'll try
(setq desktop-save-mode nil) ; fresh starts...

(setq desktop-base-file-name ".emacs.desktop"
      desktop-load-locked-desktop nil ;only have ONE main emacs open-- the rest should load quick
      desktop-files-not-to-save   "^$" ) ;reload tramp paths

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
