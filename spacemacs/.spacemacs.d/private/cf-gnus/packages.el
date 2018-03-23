(defconst cf-gnus-packages '(gnus))

(defun cf-gnus/post-init-gnus()
  (use-package gnus
    :defer t
    :config
    (define-key 'gnus-article-mode-map "}" evil-forward-paragraph)
    (define-key 'gnus-article-mode-map "{" evil-backward-paragraph)

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
                    (nnimap-stream ssl))
            ))
    ;; Send email via Gmail:
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-default-smtp-server "smtp.gmail.com")

    ;; Archive outgoing email in Sent folder on imap.gmail.com:

    ;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
    ;;       gnus-message-archive-group "[Gmail]/Sent Mail")


    ;; set return email address based on incoming email address
    ;; (setq gnus-posting-styles
    ;;       '(((header "to" "address@outlook.com")
    ;;          (address "address@outlook.com"))
    ;;         ((header "to" "address@gmail.com")
    ;;          (address "address@gmail.com"))))

    ;; store email in ~/gmail directory
    (setq message-directory "~/news/gmail"

          gnus-directory "~/news/"
          gnus-kill-files-directory "~/news/"
          gnus-home-directory "~/news/"
          gnus-dribble-directory "~/news/"
          gnus-always-read-dribble-file t
          )


    (setq
     ;; gnus-ignored-from-addresses "cfindeisen7@gmail.com"

     ;; agent
     ;; only DL unread msgs
     gnus-agent-consider-all-articles nil
     gnus-agent-mark-unread-after-downloaded t
     gnus-auto-goto-ignores 'undownloaded

     gnus-fetch-old-headers nil

     gnus-thread-hide-subtree nil

     gnus-agent t
     gnus-agent-cache t
     ;; gnus-asynchronous t
     gnus-agent-expire-days 21
     gnus-agent-synchronize-flags t
     gnus-agent-enable-expiration 'ENABLE


     message-kill-buffer-on-exit t
     ;; You need to replace this key ID with your own key ID!
     ;; mml2015-signers '("7893C0FD")

     ;; This tells Gnus to use the Gmail SMTP server. This
     ;; automatically leaves a copy in the Gmail Sent folder.
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587

     ;; Tell message mode to use SMTP.
     ;; message-send-mail-function 'smtpmail-send-it

     gnus-no-groups-message "No messages to be displayed. Get back to work"
     ;; This is where we store the password.
     nntp-authinfo-file "~/.authinfo"
     ;; Gmail system labels have the prefix [Gmail], which matches
     ;; the default value of gnus-ignored-newsgroups. That's why we
     ;; redefine it.
     gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
     ;; We don't want local, unencrypted copies of emails we write.
     ;; gnus-message-archive-group nil

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

    ;; ;; Attempt to encrypt all the mails we'll be sending.
    ;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)

    ;; ;; Add two key bindings for your Gmail experience.
    ;; (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)
    (setq gnus-group-line-format "%M%S%p%P%5y:%B %G\n")

(defhydra hydra-summary-limit-menu (:color pink
                                           :hint nil)
  "
^Limit to^             ^Expand^           ^Quit
^^^^^^^^-----------------------------------------------------------------
_s_: subject           _/_: pop-limit        _q_: quit
_a_: author            _D_: include-dormant
_r_: recipient         _g_: insert-new
_A_: address
_m_: marks
_u_: unread
"
  ("s" gnus-summary-limit-to-subject)
  ("a" gnus-summary-limit-to-author)
  ("r" gnus-summary-limit-to-recipient)
  ("A" gnus-summary-limit-to-address)
  ("m" gnus-summary-limit-to-marks)
  ("/" gnus-summary-pop-limit)
  ("u" gnus-summary-limit-to-unread)
  ("D" gnus-summary-limit-include-dormant)
  ("g" gnus-summary-insert-new-articles)
  ("q" quit-window "quit" :color blue))

(define-key gnus-summary-mode-map "/" 'hydra-summary-limit-menu/body)

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
    (add-hook 'message-mode-hook 'epa-mail-mode)
    )
  )





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
