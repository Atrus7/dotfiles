;;; Daemon code
(when (spacemacs/system-is-linux)
  (if (daemonp)
      (progn
        (message "Server Configuring.")
        (setq server-log 1)
        ;; (add-hook 'after-make-frame-functions 'cf/server-configure t)

        ;; How do we do this without ruining buffers?
        (defadvice my-desktop-restore-file-buffer-advice
            (around my-desktop-restore-file-buffer)
          "Be non-interactive while starting a daemon."
          (let ((noninteractive t))
            ad-do-it))
        (ad-activate 'my-desktop-restore-file-buffer-advice)))



  (progn
    (message "Regular Configuring.")
    )
  )
