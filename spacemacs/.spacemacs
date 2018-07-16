;;; Dotspacemacs
;;; Author: Chris Findeisen

(defun dotspacemacs/init ()
  "Spacemacs core settings."
  (dotspacemacs/init/pre-init)
  (dotspacemacs/init/coding)
  (dotspacemacs/init/display)
  (dotspacemacs/init/evil)
  (dotspacemacs/init/keys)
  (dotspacemacs/init/layouts)
  (dotspacemacs/init/misc)
  (dotspacemacs/init/packages)
  (dotspacemacs/init/startup))

(defun dotspacemacs/init/pre-init ()

  (let ((work-file "~/dotfiles/work/work-init.el"))
    (defvar at-work (file-exists-p work-file))

    (if at-work
        (progn
          (require 'work-init work-file)
          (cf/work-pre-loading))

      )
    ))

(defun dotspacemacs/layers ()
  "Spacemacs layers declarations and package configurations."
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages))

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (setq custom-file "~/.spacemacs.d/.custom-settings.el"))

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/load-credentials)
  (dotspacemacs/user-config/experiments))

;;; Spacemacs/Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '(cf-calendar
    cf-desktop
    cf-gnus
    cf-irc
    cf-linux
    cf-lisp
    cf-mac
    cf-opinions
    cf-org
    cf-scheme
    cf-ssh
    cf-writing
    )
  "Local layers housed in '~/.spacemacs.d/private'.")

(defvar dotspacemacs/layers/better-be-local
  '(syntax-checking
    ycmd)
  "The packages that are tramp-killers. Only load them if you're editing locally")

;;;; Core
(defvar dotspacemacs/layers/core
  '((auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)

    (cscope :variables
            cscope-initial-directory "~/tmp/cscope/./"
            cscope-program "/usr/bin/cscope"
            cscope-display-cscope-buffer t
            cscope-option-do-not-update-database t)
    erc
    helm ;; ivy
    git
    org
    (shell :variables
           shell-default-shell 'shell)
    spell-checking
    version-control
    )
  "Layers I consider core to Spacemacs")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(
    (c-c++ :variables
           c-c++-default-mode-for-headers 'c++-mode
           c-c++-enable-clang-support t)
    common-lisp
    emacs-lisp
    extra-langs ;; arduino
    html
    javascript
    markdown
    csv
    python
    scheme
    vimscript
    yaml
    )
  "Programming and markup language layers")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '( ;; today?
    gnus
    graphviz
    ibuffer
    )
  "Miscellaneous layers")

;;;; Layers/config

(defun dotspacemacs/layers/config ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t

   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private/")
   dotspacemacs-configuration-layers (append
                                      dotspacemacs/layers/core
                                      dotspacemacs/layers/langs
                                      dotspacemacs/layers/extra
                                      dotspacemacs/layers/local
                                      (if at-work '(cf-work) '(cf-home))
                                      (if (and at-work (spacemacs/system-is-linux))
                                          dotspacemacs/layers/better-be-local
                                          )
                                      )
   ))

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages '(solarized-theme
                                      nord-theme
                                      ;; TODO remove once this is mainlined...
                                      yasnippet-snippets
                                      )
   dotspacemacs-excluded-packages '(org-pomodoro)
   dotspacemacs-frozen-packages '()
   dotspacemacs-install-packages 'used-only
   ))

;;; Spacemacs/Init
;;;; Coding

(defun dotspacemacs/init/coding ()
  (setq-default
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-smooth-scrolling t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-line-numbers nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

;;;; Display

(defun dotspacemacs/init/display ()
  (setq-default
   dotspacemacs-themes '(
                         spacemacs-dark
                         spacemacs-light
                         nord
                         solarized-light
                         solarized-dark
                         )
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme '(spacemacs :separator bar :separator-scale 1.0)
   ))

;;;; Evil

(defun dotspacemacs/init/evil ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   ))

;;;; Keys

(defun dotspacemacs/init/keys ()
  (setq-default
   dotspacemacs-leader-key "SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key ","
   dotspacemacs-ex-command-key ":"
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-distinguish-gui-tab t
   ))

;;;; Layouts

(defun dotspacemacs/init/layouts ()
  (setq-default
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout t
   dotspacemacs-auto-resume-layouts nil
   ))

;;;; Misc

(defun dotspacemacs/init/misc ()
  (setq-default
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-persistent-server nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   ))

;;;; Packages

(defun dotspacemacs/init/packages ()
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/") t)
  (setq-default
   dotspacemacs-default-package-repository nil
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-delete-orphan-packages t
   ;; dotspacemacs-use-spacelpa nil
   ;; dotspacemacs-verify-spacelpa-archives nil
   ))

(defun dotspacemacs/init/startup ()
  (setq-default
   dotspacemacs-verbose-loading nil
   spacemacs-buffer-logo-title "[ Welcome Home ]"
   dotspacemacs-startup-banner (concat dotspacemacs-directory "img/banner.png")
   dotspacemacs-startup-lists '(agenda bookmarks recents)
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-loading-progress-bar t
   ))

;;; Spacemacs/User-Config
;;;; Toggles

(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended to be put into layers."
  (spacemacs/toggle-mode-line-minor-modes-off)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable))


;;;; Load Credentials

(defun dotspacemacs/user-config/load-credentials ()
  (when (file-exists-p (expand-file-name "~/corporate-secrets/credentials.el"))
    (load "~/corporate-secrets/credentials.el")
    ))

;;;; Experiments

(defun dotspacemacs/user-config/experiments ()
  (if at-work (cf/work-post-loading))
  (savehist-mode nil)

  ;; Prevent org capture from warning in a perspective
  (setq persp-kill-foreign-buffer-action nil)

  (spacemacs/toggle-aggressive-indent-globally-on)

  (add-hook 'c++-mode-hook
            (lambda ()
              (setq company-clang-arguments '("-std=c++11"))
              (setq flycheck-clang-language-standard "c++11")))

  )
