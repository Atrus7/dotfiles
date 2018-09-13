(defconst cf-mail-packages '(mu4e
                             gnus
                             bbdb-vcard))

;;; mu4e
(defun cf-mail/post-init-mu4e()
  (use-package mu4e
    :defer t
    :config
    ;; Basic settings
    (setq mu4e-maildir "~/maildir"
          mu4e-trash-folder "/trash"
          mu4e-refile-folder "/archive"
          mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 300
          mu4e-compose-signature-auto-include t
          mu4e-view-show-images t
          mu4e-view-image-max-width 800
          mu4e-view-show-addresses t
          mu4e-sent-messages-behavior 'delete ;; gmail/IMAP takes care of this
          mu4e-attachment-dir "~/downloads"

          mu4e-html2text-command 'my-render-html-message ;; HTML Viewing
          ;; Don't ask to quit... why is this the default?
          mu4e-confirm-quit nil)

    (evilified-state-evilify-map
      mu4e-view-mode-map
      :mode mu4e-view-mode
      :bindings
      (kbd "J") 'mu4e-view-headers-next
      (kbd "K") 'mu4e-view-headers-prev
      (kbd "C-h") help-map
      (kbd "RET") 'mu4e~view-browse-url-from-binding
      (kbd "|") 'mu4e-view-pipe
      (kbd "t") 'mu4e-view-mark-thread
      (kbd "T") 'mu4e-view-mark-subthread)

    (evilified-state-evilify-map
      mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "C-h") help-map
      (kbd "t") 'mu4e-headers-mark-thread
      (kbd "T") 'mu4e-headers-mark-subthread)

    (setq
     ;; Tell message mode to use SMTP.
     send-mail-function		nil
     message-send-mail-function	'smtpmail-send-it
     smtpmail-default-smtp-server "smtp.gmail.com"

     ;; This tells Gnus to use the Gmail SMTP server. This
     ;; automatically leaves a copy in the Gmail Sent folder.
     smtpmail-smtp-service 587)

    (setq mu4e-view-prefer-html t) ;; try to render
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t) ;; read in browser

    ;; mu4e as default email agent in emacs
    (setq mail-user-agent 'mu4e-user-agent)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
          '(
            ("/gmail/INBOX" . ?g)
            ("/gmail/[Gmail].Sent Mail" . ?s)
            ("/drafts" . ?d)
            ("/archive" . ?a)
            ("/gmail/[Gmail].Drafts" . ?D)
            ))

    ;; Bookmarks
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now" "Today's messages" ?t)
            ("date:7d..now" "Last 7 days" ?w)
            ("mime:image/*" "Messages with images" ?p)
            (,(mapconcat 'identity
                         (mapcar
                          (lambda (maildir)
                            (concat "maildir:" (car maildir)))
                          mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)))

    ;; Modeline notifications
    (with-eval-after-load 'mu4e-alert
      ;; Enable Desktop notifications
      (mu4e-alert-set-default-style 'notifications)) ; For linux
    ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux
    (setq mu4e-enable-mode-line t)

    (add-hook 'message-mode-hook 'turn-on-flyspell)

    ;; A match-all useful for marking everything
    (add-to-list 'mu4e-headers-custom-markers
                 '("ALL" (lambda (msg &optional param) t) (lambda () nil)))

    ;; attachments from gnus
    (require 'gnus-dired)
    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

    (spacemacs|define-custom-layout "mail"
      :binding "m"
      :body
      ;; set OS Notifications just before launching
      (setq mu4e-enable-notifications t)
      (mu4e-update-mail-and-index t)
      (mu4e)
      )))

(defun cf-mail/init-bbdb-vcard()
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

;;; GNUs setup
(defun cf-mail/pre-init-gnus()
  (use-package gnus
    :defer t
    :config
    (setq
     ;; don't read or write newsrc files
     gnus-save-newsrc-file nil
     gnus-read-newsrc-file nil)
    ))


(defun cf-mail/post-init-gnus()
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

    ;; ;; Send email via Gmail:
    ;; (setq
    ;;  ;; Tell message mode to use SMTP.
    ;;  send-mail-function		nil
    ;;  message-send-mail-function	'smtpmail-send-it
    ;;  smtpmail-default-smtp-server "smtp.gmail.com"

    ;;  ;; This tells Gnus to use the Gmail SMTP server. This
    ;;  ;; automatically leaves a copy in the Gmail Sent folder.
    ;;  smtpmail-smtp-service 587)

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


;; TODO encryption??
;; We don't want local, unencrypted copies of emails we write.
;; gnus-message-archive-group nil

;; (add-hook 'message-mode-hook 'epa-mail-mode)

;; ;; Attempt to encrypt all the mails we'll be sending.
;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)
