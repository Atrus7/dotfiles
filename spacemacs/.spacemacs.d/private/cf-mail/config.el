(setq
 ;;; Sending mail stuff
 ;; Tell message mode to use SMTP.
 send-mail-function		nil
 message-send-mail-function	'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"

 ;; This tells Gnus to use the Gmail SMTP server. This
 ;; automatically leaves a copy in the Gmail Sent folder.
 smtpmail-smtp-service 587

 ;;; Misc
 ;; Shr has really bad colors in html emails
 shr-use-colors nil
 nntp-authinfo-file "~/.authinfo.gpg")
