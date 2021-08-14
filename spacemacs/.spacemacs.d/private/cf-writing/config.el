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

(defvar writing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    (define-key map [?-] 'typopunct-insert-typographical-dashes)
    map))

(define-minor-mode cf/writing-mode ()
  :keymap writing-mode-map
  :group 'writing
  :global nil

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

  ;; Spelling stuff
  (flyspell-mode-on)

  nil " Writing" '()
  )

;; setup writing mode when we're in some writing/ dir
(add-hook 'org-mode-hook
          (lambda ()
            (if (and (stringp buffer-file-name)
                     (string-match "/writing/" buffer-file-name))
                (cf/writing-mode))) t)


;; https://www.reddit.com/r/emacs/comments/4oc7pg/spellcheck_flyspellmode_underlines_disappear_when/
;; REDEFINE THIS FN, so flyspell doesn't de-activate.
(defun ispell-pdict-save (&optional no-query force-save)
  "CF-REDEFINED -- Check to see if the personal dictionary has been modified.
If so, ask if it needs to be saved."
  (interactive (list ispell-silently-savep t))
  (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
      (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
  (when (and (or ispell-pdict-modified-p force-save)
	           (or no-query
		             (y-or-n-p "Personal dictionary modified.  Save? ")))
    (ispell-send-string "#\n")	; save dictionary
    (message "Personal dictionary saved.")
    ;; - (when flyspell-mode
    ;;- (flyspell-word nil)
    ;;- )
    )
  ;; unassert variable, even if not saved to avoid questioning.
  (setq ispell-pdict-modified-p nil))
