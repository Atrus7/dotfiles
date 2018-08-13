;; CONFLICT~/.gnus is by default the gnus-init-file
;;gnus-startup-file "~/.gnus/.newsrc"
;;gnus-init-file "~/.gnus/.gnus"


;; Perhaps have a system-message function that calls notify-send with the given message and also writes to normal message buffer
;; In this case you could message when Gnus starts stawrting and when it finishes starting up.
;; ‘gnus-before-startup-hook’
;; ‘gnus-startup-hook’


;; Unbind this key; it's annoying!
;; (define-key gnus-summary-mode-map "o" (lambda () (interactive)))

(defun cf/get-new-news-and-disconnect (&optional arg)
  "Plug in, send, receive, plug out."
  (interactive "P")
  (gnus-group-save-newsrc)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-group-get-new-news arg)
  (gnus-agent-fetch-session)
  (gnus-group-save-newsrc)
  (gnus-agent-toggle-plugged nil))


(setq
 ;; Shr has really bad colors in html emails
 shr-use-colors nil

 ;; Tell message mode to use SMTP.
 send-mail-function		nil
 message-send-mail-function	'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"

 ;; This tells Gnus to use the Gmail SMTP server. This
 ;; automatically leaves a copy in the Gmail Sent folder.
 smtpmail-smtp-service 587)

(setq nntp-authinfo-file "~/.authinfo.gpg")

;; (setq gnus-parameters '((".*" (banner . iphone)))
;; gnus-article-banner-alist '((iphone . "\\(^Sent from my iPhone$\\)"))
;; gnus-use-full-window nil)

;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (define-key gnus-group-mode-map (kbd "vo")
;;   '(lambda ()
;;      (interactive)
;;      (shell-command "offlineimap&" "*offlineimap*" nil)))


;; ;; eshell
;; (def-persp-buffer-save/load
;;  :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
;;  :save-vars '(major-mode default-directory))

;; ;; compile
;; (def-persp-buffer-save/load
;;  :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
;;  :save-vars '(major-mode default-directory compilation-directory
;;                          compilation-environment compilation-arguments))
;; ;; ()
;; ;; (persp-load-)

;; ;; magit-status
;; (with-eval-after-load "magit-autoloads"
;;   (autoload 'magit-status-mode "magit")
;;   (autoload 'magit-refresh "magit")
;;   (def-persp-buffer-save/load
;;    :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
;;    :save-vars '(major-mode default-directory)
;;    :after-load-function #'(lambda (b &rest _)
;;                             (with-current-buffer b (magit-refresh)))))
;; (lambda (savelist)
;;   (when (eq (car savelist) 'def-ielm-buffer)
;;     (with-current-buffer (get-buffer-create (cadr savelist))
;;       (setq default-directory (caddr savelist))
;;       (require 'ielm)
;;       (inferior-emacs-lisp-mode))))
