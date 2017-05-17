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
(setq erc-part-reason-normal "")
(setq erc-truncate-mode 1)

;; share my real name
(setq erc-user-full-name "Chris Findeisen")

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"));"324" "329" "332" "333" "353" "477"

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#scheme" "#lisp")))
