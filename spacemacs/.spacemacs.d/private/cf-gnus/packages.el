(defconst cf-gnus-packages '(gnus
                             bbdb-vcard))

(defun cf-gnus/pre-init-gnus()
  (use-package gnus
    :defer t
    :config
    (setq
    ;; don't read or write newsrc files
    gnus-save-newsrc-file nil
    gnus-read-newsrc-file nil)
    ))


(defun cf-gnus/init-bbdb-vcard()
  (use-package bbdb-vcard
    :defer t
    :config
    ;; import file
    (bbdb-vcard-import-file "~/news/bbdb/contacts.bbdb")
    (setq bbdb-message-pop-up nil
          bbdb-mua-pop-up nil
          bbdb-complete-mail-allow-cycling t)

    (bbdb-initialize 'message 'gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

    )
  )

(defun cf-gnus/post-init-gnus()
  (use-package gnus
    :defer t
    :config

    ;; Get email, and store in nnml
    (setq gnus-secondary-select-methods
          '(
            ;; (nntp "gmane"
            ;;       (nntp-address "news.gmane.org"))
            ;; (nntp "news.eternal-september.org")
            ;; (nntp "nntp.aioe.org")
            ;; (nntp "news.gwene.org")
            (nnimap "mail"
                    (nnimap-address "imap.gmail.com")
                    (nnimap-server-port 993)
                    (nnimap-stream ssl)
                    (nnir-search-engine imap)
                    ))
          )

    ;; Gnus search with gmail args
    (setq nnir-imap-default-search-key "gmail")
    ;; (add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))

    ;; Send email via Gmail:
    (setq
     ;; Tell message mode to use SMTP.
          send-mail-function		nil
          message-send-mail-function	'smtpmail-send-it
          smtpmail-default-smtp-server "smtp.gmail.com"

          ;; This tells Gnus to use the Gmail SMTP server. This
          ;; automatically leaves a copy in the Gmail Sent folder.
          smtpmail-smtp-service 587)

    ;; Archive outgoing email in Sent folder on imap.gmail.com:

    (setq gnus-message-archive-method
          '(nnfolder "archive"
                     (nnfolder-inhibit-expiry t)
                     (nnfolder-active-file "~/news/sent-mail/active")
                     (nnfolder-directory "~/news/sent-mail/")

                     )
          gnus-message-archive-group "[Gmail]/Sent Mail")


    ;; set return email address based on incoming email address
    ;; (setq gnus-posting-styles
    ;;       '(((header "to" "address@outlook.com")
    ;;          (address "address@outlook.com"))
    ;;         ((header "to" "address@gmail.com")
    ;;          (address "address@gmail.com"))))

    ;; store email in ~/news stuff... directory
    (setq message-directory "~/news/gmail"
          gnus-directory "~/news/"
          gnus-kill-files-directory "~/news/"
          gnus-home-directory "~/news/"
          gnus-dribble-directory "~/news/"
          gnus-always-read-dribble-file t
          nnmail-spool-hook "~/news/spoolfile")

    (setq
     ;; agent
     ;; only DL unread msgs

     ;; email signature
     message-signature-file "~/news/.signature_regards"
     message-forward-before-signature t


     gnus-agent-consider-all-articles nil
     gnus-agent-mark-unread-after-downloaded t
     gnus-auto-goto-ignores 'undownloaded

     gnus-fetch-old-headers nil

     ;; fetch only partial articles
     gnus-read-active-file 'some

     gnus-thread-hide-subtree nil

     ;; gnus-agent t
     ;; gnus-agent-cache t
     ;; gnus-asynchronous t
     ;; gnus-agent-expire-days 21
     ;; gnus-agent-synchronize-flags t
     ;; gnus-agent-enable-expiration 'ENABLE


     ;; message-kill-buffer-on-exit t

     ;; You need to replace this key ID with your own key ID!
     ;; mml2015-signers '("7893C0FD")


     gnus-no-groups-message "No messages to be displayed. Get back to work"
     ;; This is where we store the password.
     ;; Gmail system labels have the prefix [Gmail], which matches
     ;; the default value of gnus-ignored-newsgroups. That's why we
     ;; redefine it.
     gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"


     gnus-simplify-ignored-prefixes (concat
            "\\`\\[?\\("
            (mapconcat
             'identity
             '("looking"
               "wanted" "followup" "summary\\( of\\)?"
               "help" "query" "problem" "question"
               "answer" "reference" "announce"
               "How can I" "How to" "Comparison of"
               ;; ...
               )
             "\\|")
            "\\)\\s *\\("
            (mapconcat 'identity
                       '("for" "for reference" "with" "about")
                       "\\|")
            "\\)?\\]?:?[ \t]*"))


    ;; ;; Add two key bindings for your Gmail experience.
    ;; (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)
    (setq gnus-group-line-format "%M%S%p%P%5y:%B %G\n")


    (setq
     ;; graphics
     gnus-auto-center-summary t

     gnus-score-over-mark ?\u2191          ; \u2191 \u2600
     gnus-score-below-mark ?\u2193         ; \u2193 \u2602
     gnus-ticked-mark ?\u2691
     gnus-dormant-mark ?\u2690
     gnus-expirable-mark ?\u267b
     gnus-read-mark ?\u2713
     gnus-del-mark ?\u2717
     gnus-killed-mark ?\u2620
     gnus-replied-mark ?\u27f2
     gnus-forwarded-mark ?\u2933
     gnus-cached-mark ?\u260d
     gnus-recent-mark ?\u2605
     gnus-unseen-mark ?\u2729
     gnus-unread-mark ?\u2709
     gnus-summary-line-format (concat "%{|%}"
                                      "%U%R%z"
                                      "%{|%}"
                                      "%O"
                                      "%{|%}"
                                      "%(%-18,18f"
                                      "%{|%}"
                                      "%*%{%B%} %s%)"
                                      "\n"))

    ;; (gnus-demon-init)
    ;; (gnus-demon-add-handler 'gnus-group-get-new-news 10 t)

    (add-hook 'message-mode-hook 'turn-on-flyspell)

    ;; display-time-mode mail notification
    (defface display-time-mail-face '((t (:background "red")))
      "If display-time-use-mail-icon is non-nil, its background colour is that
      of this face. Should be distinct from mode-line. Note that this does not seem
      to affect display-time-mail-string as claimed.")
    (setq
     display-time-mail-file "~/news/spoolfile"
     display-time-use-mail-icon t
     display-time-mail-face 'display-time-mail-face
     message-fill-column 120)

    (define-key gnus-article-mode-map "}" 'evil-forward-paragraph)
    (define-key gnus-article-mode-map "{" 'evil-backward-paragraph)

    (define-key gnus-group-mode-map
      ;; list all the subscribed groups even they contain zero un-read messages
      (kbd "s") 'gnus-group-make-nnir-group)

    ;; (display-time-mode t)
))






;; (defun my-gnus-summary-keys ()
;;   (local-set-key "y" 'gmail-archive)
;;   (local-set-key "$" 'gmail-report-spam))

;; (defun gmail-archive ()
;;   "Archive the current or marked mails.
;; This moves them into the All Mail folder."
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

;; (defun gmail-report-spam ()
;;   "Report the current or marked mails as spam.
;; This moves them into the Spam folder."
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))
;; )


;; TODO encryption??
;; We don't want local, unencrypted copies of emails we write.
;; gnus-message-archive-group nil

;; (add-hook 'message-mode-hook 'epa-mail-mode)

;; ;; Attempt to encrypt all the mails we'll be sending.
;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)
;; (evilified-state--define-pre-bindings)
