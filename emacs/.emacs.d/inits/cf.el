;;; cf.el --- Collection of functions that I wrote or pulled from others, not belonging to a bigger library
;;; Commentary:

;; I need to pull out the emacs frame hook and font stuff to handle it later

;; macro used to wrap stuff that need to be run only after emacs
;; starts up completely. This is very crucial when calling functions like
;; `find-font' which return correct value only after emacs startup is finished
;; especially when emacs is started in daemon mode.
(defmacro do-after-full-emacs-start (&rest body)
  `(run-with-idle-timer 1 ; run this after emacs is idle for 1 second
                        nil ; do this just once; don't repeat
                        (lambda () ,@body)))



(defun cf/set-frame-font (face default-size &optional given-size) ;; pulled from hershel bhave
  (setq new-font-size
        (if given-size
            (if (numberp given-size) (number-to-string given-size) (given-size))
          (if (numberp default-size) (number-to-string default-size) (default-size))))

  (setq default-frame-alist (list (cons 'font (concat face " " new-font-size))))
  (set-frame-font (concat face " " new-font-size)))


(defun hack-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Hack" 7 size))
(defun meslo-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Meslo LG S" 7 size))
(defun menlo-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Menlo" 7 size))
(defun bitstream-face (&optional size)
  (interactive "N")
  (cf/set-frame-font "Bitstream Vera Sans Mono" 7 size))
(defun courier-face ()
  (interactive)
  (set-frame-font "-adobe-courier-medium-r-normal-*-*-*-100-100-*-60-iso10646-1"))


(defun cf/server-configure(_)
  (do-after-full-emacs-start (cf/configure-os-linux))
  )

(defun cf/configure-os-linux ()
  (cond
   ((find-font (font-spec :name "Hack"))
    (hack-face))
   ((find-font (font-spec :name "Menlo"))
    (menlo-face))
   ((find-font (font-spec :name "Meslo LG S"))
    (meslo-face))
   ((find-font (font-spec :name "Bitstream Vera Sans Mono"))
    (bitstream-face))
   ((find-font (font-spec :name "-adobe-courier-medium-r-normal-*-*-*-100-100-*-60-iso10646-1"))
    (courier-face)))
  )


;;; Mac specific
(when (eq system-type 'darwin)
  (message "Darwin Recognized.")
                                        ;(add-hook 'after-make-frame-functions 'cf/configure-os-darwin t)
  (cf/configure-os-darwin)
  )

;;;Linux specific
(when (eq system-type 'gnu/linux)
  (require 'server)
  (message "Linux Recognized.")
  (if (daemonp)
      (progn
        (message "Server Configuring.")
        (setq server-log 1)
        (add-hook 'after-make-frame-functions 'cf/server-configure t))
    (progn
      (message "Regular Configuring.")
      (cf/configure-os-linux))
    )

  )



(el-init-provide)
;;; cf.el ends here
