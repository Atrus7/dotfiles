;;; packages.el --- cf-writing layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Christopher Findeisen <Chris@Mack.local>
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
;; added to `cf-writing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cf-writing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cf-writing/pre-init-PACKAGE' and/or
;;   `cf-writing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cf-writing-packages
  '(nanowrimo
    visual-fill-column
    olivetti
    poet-theme
    )
  "The list of Lisp packages required by the cf-writing layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(spacemacs|add-toggle cf/writing-mode
  :status cf/writing-mode
  :on (progn (cf/writing-mode 1)
             (load-theme 'poet t)
             )
  :off (progn (cf/writing-mode -1)
              (load-theme (car dotspacemacs-themes) t)
              )
  )


(define-minor-mode cf/writing-mode ()
  nil " Writing" '() :group 'writing)


(defun cf-writing/init-nanowrimo ()
  (use-package nanowrimo
    :defer t
    :init
    )
  )

(defun cf-writing/init-visual-fill-column ()
  (use-package visual-fill-column
    :defer t
    :init
    )
  )

(defun cf-writing/init-olivetti ()
  (use-package visual-fill-column
    :defer t
    :config
    (setq-default
     olivetti-minimum-body-width 80
    olivetti-body-width 120)
    (spacemacs/set-leader-keys (kbd "w c") 'olivetti-mode))
  )

(defun cf-writing/init-poet-theme ()
  (use-package poet-theme
    :defer t
    :config
    )
  )

;;; packages.el ends here
