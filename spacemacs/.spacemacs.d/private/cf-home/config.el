;; OK, are we on the laptop, or desktop?
(setq at-desk (string-equal (system-name) "desk"))
(if at-desk
    (progn (load-theme 'cherry-blossom t)
           (with-eval-after-load "pdf-tools" (pdf-tools-install))

           (message "At my desktop.")
           )
  (progn
    (message "At laptop."))
  )


;; To be the default opinions of my home setup. Particular to outside programs that are installed on my machine other than emacs.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; gnus stuff
(setq
 user-full-name "Chris Findeisen"
 user-mail-address "cfindeisen7@gmail.com")
