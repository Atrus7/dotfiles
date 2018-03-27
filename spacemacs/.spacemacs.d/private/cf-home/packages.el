(defconst cf-home-packages '(gnus))

(defun cf-home/post-init-gnus()
  (use-package gnus
    :defer t
    :config
    (setq nntp-authinfo-file "~/.authinfo.gpg")
    ))
