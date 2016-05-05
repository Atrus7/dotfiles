;;; Commentary: General sane settings. These are just the first things we want to start up when we set up Emacs.

(show-paren-mode 1)
(global-linum-mode 1) ; display line numbers
(linum-on) ; coming from picky-linum
(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-battery-mode 1)
(scroll-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p) ; Changes all yes/no questions to y/n type

(setq visible-bell 1 ; visual rather than auditory
      smooth-scroll-margin 2
      )

(el-init-provide)
