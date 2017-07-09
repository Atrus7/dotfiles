;;; irc configs
(setq erc-nick "atrus7")

;; inspired from https://github.com/bbatsov/emacs-dev-kit/blob/master/erc-config.el

(when (file-exists-p (expand-file-name "~/.erc_pass.el"))
  (load "~/.erc_pass.el")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode (("atrus7" . ,erc-pass)))))
)
(setq erc-truncate-mode 1)

;; share my real name
(setq erc-user-full-name "Chris Findeisen")

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"));

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#lisp" "#digitalstation", "#junto")))

(defun call-libnotify (matched-type nick msg)
  (let* ((cmsg  (split-string (clean-message msg)))
         (nick   (first (split-string nick "!")))
         (msg    (mapconcat 'identity (rest cmsg) " ")))
    (shell-command-to-string
     (format "notify-send -u critical '%s says:' '%s'" nick msg))))



;; how long before a similar nick can create a notification
(setq erc-notify-timeout 5)

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

;; private message notification
(defun erc-notify-on-private-msg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-notify-allowed-p nick))
      (shell-command-to-string
       (format "notify-send -u critical '%s says:' '%s'" nick msg))
      nil)))

(setq cf/erc-part-default-reason "See ya")
(setq cf/erc-quit-default-reason "RL calls")

;; Don't prompt for reasons
(defun cf/erc-part-from-channel ()
  (interactive)
  (erc-part-from-channel cf/erc-part-default-reason))
(defun cf/erc-quit-server ()
  (interactive)
  (erc-quit-server cf/erc-quit-default-reason))

(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-on-private-msg)
