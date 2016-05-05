;;; Commentary: Pretty and more opinionated Settings. Make it great!
;; Relative line numbering
(require 'cf-sanity)
(require 'cf-evil)
(require 'linum-relative)
(require 'theme-changer) ; Let it be stark when it's dark, and light when it's bright
(require 'guide-key)
(require 'crosshairs)
(require 'diminish)

;;; Theme-changer -- I like colors.
(setq calendar-location-name "Austin, TX")
(setq calendar-latitude [30 18 north] )
(setq calendar-longitude [97 44 west] )
(change-theme 'gruvbox 'solarized-light)
;(setq solarized-distinct-fringe-background nil)
;(setq solarized-high-contrast-mode-line t)
;(setq solarized-use-more-italic)


;; Linum-relative
(setq linum-relative-current-symbol "")

;; Guide-key
(setq guide-key/guide-key-sequence '("C-c" "C-x" "C-x r" "C-c p"))
;(setq guide-key/highlight-command-regexp
      ;'("rectangle"
        ;("C" . "hot-pink")))
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("helm" . "hot pink")))
(guide-key-mode 1)

;; Crosshairs: For finding cursor
(toggle-crosshairs-when-idle 1) ;~ 5 sec
(setq col-highlight-vline-face-flag  t
      col-highlight-face             hl-line-face)
(global-hl-line-mode 1)


(setq evil-insert-state-cursor '((bar . 5) "green")
      evil-normal-state-cursor '(box "purple")
      evil-visual-state-cursor '(box "yellow")
      evil-replace-state-cursor '(box "red")
      )



;; Diminish: Mode line should look nicer...


(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "projectile"
'(diminish 'projectile-mode (format " P:|%s|" (projectile-project-name)))
)

(el-init-provide)
;;; cf-pretty ends here
