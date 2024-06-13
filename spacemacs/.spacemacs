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
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (let ((work-file "~/dotfiles/work/work-init.el"))
    (defvar at-work (file-exists-p work-file))
    (if at-work
        (progn
          (require 'work-init work-file)
          (cf/work-pre-loading)
          )
      )
    )
  )

(defun dotspacemacs/layers ()
  "Spacemacs layers declarations and package configurations."
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages)
  )


(defun cf/external-packages ()
  "Elisp that I rolled from somewhere other than elpa and distribute in dotfiles."
  (add-to-list 'load-path "~/.spacemacs.d/external/")
  (add-hook 'org-mode-hook 'turn-on-auto-capitalize-mode)

  ;; TODO improve loading of local-external packages
  (require 'auto-capitalize)
  (require 'hl-line+)

  (add-to-list 'load-path "~/.spacemacs.d/external/org-marginalia/")
  (require 'org-marginalia)
  )

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (add-to-list 'configuration-layer-elpa-archives
               '("ox-odt" . "https://kjambunathan.github.io/elpa/"))
  (setq custom-file "~/.spacemacs.d/.custom-settings.el"))

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/load-credentials)
  (cf/external-packages)
  (dotspacemacs/user-config/experiments) ;; the very last thing
  )

;;; Spacemacs/Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '(util-funcs
    cf-cc
    cf-calendar
    cf-desktop
    ;; cf-mail
    cf-irc
    cf-ide
    cf-linux
    cf-lisp
    cf-mac
    cf-opinions
    cf-org
    cf-py
    cf-scheme
    cf-ssh
    cf-writing
    )
  "Local layers housed in '~/.spacemacs.d/private'.")

(defvar dotspacemacs/layers/better-be-local
  '(syntax-checking)
  "The packages that are tramp-killers. Only load them if you're editing locally")

;;;; Core
(defvar dotspacemacs/layers/core
  '((auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    helm
    git
    (org :variables
         org-want-todo-bindings t)
    (deft :variables
      deft-zetteldeft t)
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
    haskell
    html
    javascript
    swift
    markdown
    systemd
    csv
    (spell-checking :variables spell-checking-enable-by-default nil)
    ;; extra-langs ;; AKA "extra-langs" on master
    (python :variables python-shell-interpreter "ipython3"
            python-backend 'anaconda
            python-formatter 'yapf
            python-format-on-save t
            )
    scheme
    vimscript
    yaml
    )
  "Programming and markup language layers")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '( ;; today?
    dash
    erc
    ;; gnus
    ;; (mu4e :variables
    ;;       mu4e-installation-path "/usr/share/emacs/site-lisp")
    graphviz
    ibuffer
    )
  "Miscellaneous layers")

;;;; Experiments...

(defvar dotspacemacs/layers/experimental
  '(semantic))

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
                                      dotspacemacs/layers/experimental
                                      (if at-work '(
                                                    cf-work
                                                    cast) '(cf-home))
                                      (if (spacemacs/system-is-linux)
                                          dotspacemacs/layers/better-be-local
                                        )
                                      dotspacemacs/layers/local
                                      )
   ))

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages '(;; doom-modeline
                                      arduino-mode
                                      calfw
                                      calfw-org
                                      doom-themes
                                      evil-snipe
                                      highlight
                                      langtool
                                      modus-themes
                                      ninja-mode
                                      nord-theme
                                      org-fancy-priorities
                                      ox-odt
                                      pdf-tools ;; for printing
                                      solarized-theme
                                      rainbow-mode
                                      writegood-mode
                                      zetteldeft
                                      ;; TODO remove once this is mainlined...
                                      yasnippet-snippets
                                      ;; (evil-adjust :location (recipe :fetcher github :repo "troyp/evil-adjust"))
                                      )
   dotspacemacs-excluded-packages '(org-pomodoro
                                    org-projectile ; requires Emacs 28
                                    org-rich-yank ;; giving error of `'org-rich-yank--store: Symbol’s value as variable is void: element`
                                    gnuplot
                                    devdocs ;; Needs emacs-27.1
                                    treemacs-icons-dired)
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
   dotspacemacs-whitespace-cleanup 'nil
   ))

;;;; Display
(defun dotspacemacs/init/display ()
  (setq-default
   dotspacemacs-themes '(cherry-blossom
                         modus-operandi
                         leuven
                         doom-one-light
                         doom-one
                         spacemacs-dark
                         spacemacs-light
                         nord
                         solarized-light
                         solarized-dark
                         )
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   dotspacemacs-mode-line-theme '(spacemacs :separator bar :separator-scale 1.0)
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   doom-one-brighter-comments nil
   doom-one-brighter-modeline t
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
   spacemacs-buffer-logo-title "[ SYSTEM COWBOY ]"
   dotspacemacs-startup-banner (concat dotspacemacs-directory "img/banner.png")
   dotspacemacs-startup-lists '((agenda . 5) (bookmarks . 5) (recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-loading-progress-bar t
   ))

;;; Spacemacs/User-Config
;;;; Toggles

(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended to be put into layers."
  ;; TODO(uncomment, Right now causing this unfixed issued https://github.com/Malabarba/aggressive-indent-mode/issues/138)
  ;; (spacemacs/toggle-aggressive-indent-globally-on)
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

  (evil-collection-pdf-setup)
  (setq pdf-misc-print-program-executable "lpr")
  (setq pdf-misc-print-program-args '("-o media=A4" "-o fitplot"))
  (pdf-tools-install) ; Standard activation command

  ;; (doom-modeline-init)

  (add-hook 'org-mode-hook 'spacemacs/toggle-whitespace-cleanup-off)

  (defvar local-file "~/dotfiles/spacemacs/.spacemacs.d/private/local/init.el")
  ;; (if (file-exists-p)
  ;;     (require 'local-init local-file)
  ;;   )

  (setq deft-extensions '("org" "md" "txt")
        deft-directory (concat "~/org/deft/")
        deft-recursive t)
  (use-package zetteldeft
    :after deft)

  ;; freezes emacs
  (add-hook 'python-mode-hook
            (lambda ()
              (eldoc-mode -1)))

  (require 'printing)
  (pr-update-menus)

  ;; Langtool
  (setq langtool-language-tool-jar "~/keep/LanguageTool-5.7-stable/languagetool-commandline.jar"
        langtool-disabled-rules '("WHITESPACE_RULE" "SENTENCE_WHITESPACE")
        )


  ;; evil-snipe
  (evil-snipe-override-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope 'whole-visible
        evil-snipe-tab-increment t
        olivetti-style t)

  (add-to-list 'purpose-user-mode-purposes '(org-mode . writing))
  (add-to-list 'purpose-user-mode-purposes '(pdf-view-mode . writing))
  (add-to-list 'purpose-user-name-purposes '("todo.org" . agenda))
  (add-to-list 'purpose-user-name-purposes '("*Org Agenda*" . agenda))
  (purpose-compile-user-configuration)

  ;; Initial setup for layouts.
  (defvar cf/init-persp-file "/home/atrus/.spacemacs.d/private/cf-opinions/persp.el")
  (if (file-exists-p cf/init-persp-file)
      (progn (org-agenda nil "c") ;; start agenda for the persp
             (persp-mode 1)
             (persp-load-state-from-file cf/init-persp-file))
    )
  )
