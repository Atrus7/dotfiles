;;; Dotspacemacs
;;; Author: Chris Findeisen

(defun dotspacemacs/init ()
  "Spacemacs core settings."
  (dotspacemacs/init/coding)
  (dotspacemacs/init/display)
  (dotspacemacs/init/evil)
  (dotspacemacs/init/keys)
  (dotspacemacs/init/layouts)
  (dotspacemacs/init/misc)
  (dotspacemacs/init/packages)
  (dotspacemacs/init/startup))

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
    cf-irc
    cf-linux
    cf-mac
    cf-opinions
    cf-org
    cf-scheme
    cf-ssh
    cf-writing)
  "Local layers housed in '~/.spacemacs.d/private'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '(
    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    erc
    git
    org
    (shell :variables
           shell-default-shell 'eshell)
    spell-checking
    syntax-checking
    syntax-checking
    version-control
    )
  "Layers I consider core to Spacemacs")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(
    (c-c++ :variables
           c-c++-default-mode-for-headers 'c++-mode
           c-c++-enable-clang-support t)
    emacs-lisp
    html
    javascript
    markdown
    python
    scheme
    vimscript)
  "Programming and markup language layers")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '(
    ;;gnus ;;One day...
    graphviz
    ibuffer
    ;; (ibuffer :variables
    ;;          ibuffer-group-buffers-by 'projects)
    )
  "Miscellaneous layers")

;;;; Layers/config

(defun dotspacemacs/layers/config ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t

   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private/")
   dotspacemacs-configuration-layers (append dotspacemacs/layers/core
                                             dotspacemacs/layers/langs
                                             dotspacemacs/layers/extra
                                             dotspacemacs/layers/local)
   ))

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages '(solarized-theme
                                      nord-theme)
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
   ))

;;;; Evil

(defun dotspacemacs/init/evil ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-remap-Y-to-y$ nil
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
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   ))

;;;; Misc

(defun dotspacemacs/init/misc ()
  (setq-default
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-persistent-server nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   ))

;;;; Packages

(defun dotspacemacs/init/packages ()
  (setq-default
   dotspacemacs-default-package-repository nil
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-delete-orphan-packages t
   ))

;;;; Startup

(defun dotspacemacs/init/startup ()
  (setq-default
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
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
  (spacemacs/toggle-aggressive-indent-globally-on)
  ;; Prevent org capture from warning in a perspective
  (setq persp-kill-foreign-buffer-action nil)

  (add-hook 'c++-mode-hook
            (lambda ()
              (setq company-clang-arguments '("-std=c++11"))
              (setq flycheck-clang-language-standard "c++11")
              ))
  )
