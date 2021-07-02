(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(setq visual-fill-column-width 100)

(setq-default abbrev-mode t)
;; save abbreviations upon exiting xemacs
(setq save-abbrevs t)
;; set the file storing the abbreviations
(setq abbrev-file-name "~/dotfiles/spacemacs/.spacemacs.d/private/cf-writing/abbrevs.el")
;; reads the abbreviations file on startup
(quietly-read-abbrev-file)

(defvar-local local-word-count nil)

(add-hook 'after-save-hook 'cf/wc-update t)

(defvar writing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    map))

(define-minor-mode cf/writing-mode ()
  :keymap writing-mode-map
  :group 'writing
  :global t

  (setq-local org-startup-folded nil)
  (setq-local org-level-color-stars-only nil)
  (setq-local org-hide-leading-stars t)

  ;; org number headlines
  (setq-local org-num-skip-unnumbered t)
  (setq-local org-num-skip-footnotes t)
  (setq-local org-num-max-level 2)
  (setq-local org-num-face nil)
  (setq-local line-spacing 2)
  ;; Have org number headlines
  ;; (org-num-mode 1)

  ;; Org indent
  (org-indent-mode 1)

  ;; Center the buffer
  (olivetti-mode 1)
  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date startup))

  nil " Writing" '()
  )
