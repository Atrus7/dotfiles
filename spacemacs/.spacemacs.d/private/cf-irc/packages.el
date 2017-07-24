;;; packages.el --- cf-irc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <atrus@archy>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cf-irc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cf-irc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cf-irc/pre-init-PACKAGE' and/or
;;   `cf-irc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cf-irc-packages
  '(erc))

(defun cf-irc/post-init-erc ()
  (spacemacs/set-leader-keys-for-major-mode 'erc-mode
    "P" 'cf/erc-part-from-channel
    "Q" 'cf/erc-quit-server)
  )


;;; packages.el ends here
