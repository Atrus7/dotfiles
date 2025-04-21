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
    ;; Move inside of paragraphs.
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    ;; Insert em-dashes when I want to.
    (define-key map [?-] 'typopunct-insert-typographical-dashes)
    map))


(defvar editing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'langtool-goto-next-error)
    map))


;; Make better diffs for writing...
(defun my-diff-long-lines ()
  "Disable `truncate-lines' in the current buffer."
  (setq-local truncate-lines nil)
  (setq-local magit-diff-refine-hunk t))

(add-hook 'magit-status-mode-hook #'my-diff-long-lines)


(with-eval-after-load "writegood-mode"
  (add-to-list 'writegood-weasel-words "just"))

(define-minor-mode cf/editing-mode ()
  :keymap editing-mode-map
  :group 'editing
  :global nil
  ;; (writegood-mode 1)
  (langtool-check-buffer)

  (evil-define-key '(normal) org-mode-map (kbd "C-n") 'langtool-goto-next-error)
  (setq
   langtool-disabled-rules
   '("WHITESPACE_RULE" "SENTENCE_WHITESPACE" "PRP_VB" "COMMA_COMPOUND_SENTENCE" "ENGLISH_WORD_REPEAT_BEGINNING_RULE" "PRP_PAST_PART" "OUTTA")
   )
  (flyspell-buffer)
  )

(define-minor-mode cf/writing-mode ()
  :keymap writing-mode-map
  :group 'writing
  :global nil

  (setq-local org-startup-folded nil)
  (setq-local org-level-color-stars-only nil)
  (setq-local org-hide-leading-stars t)

  ;; Don't accidentally get out of my buffer...usually not doing a lot of cross-buffer jumps when writing.
  (setq-local evil-jumps-cross-buffers nil)

  ;; org number headlines
  (setq-local org-num-skip-unnumbered t)
  (setq-local org-num-skip-footnotes t)
  (setq-local org-num-max-level 1)
  (setq-local org-num-format-function #'cf/org-num-chapter-format)
  (setq-local org-num-face nil)
  (setq-local line-spacing 2)
  ;; Have org number headlines
  ;; (org-num-mode 1)

  ;; Org indent
  (org-indent-mode 1)

  ;; This interferes with olivetti.
  (visual-fill-column-mode--disable)
  ;; Center the buffer
  (olivetti-mode 1)
  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date startup))

  ;; Change something if we're in a custom writing-project
  ;; (if (and (stringp buffer-file-name)
  ;;          (string-match "/writing/projects" buffer-file-name))
  ;;     (echo "writing"))

  ;; Spelling stuff
  (turn-on-flyspell)

  (company-mode -1)

  (git-gutter-mode -1)
  nil " Writing" '()
  )

;; Performance stuff
;; this causes issues in my larger edits...
(with-eval-after-load
    "git-gutter"
  ;; (global-git-gutter-mode -1)
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode image-mode))

  )
;; (git-gutter+-mode -1)

;; setup writing mode when we're in some writing/ dir
(add-hook 'org-mode-hook
          (lambda ()
            (if (and (stringp buffer-file-name)
                     (or
                      (string-match "/org/" buffer-file-name)
                      (string-match "/writing/" buffer-file-name)))
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

(defun character-names-regex (names)
  "Create a regex pattern to match character names and their possessive forms."
  (concat "\\b\\("
          (mapconcat 'identity names "\\|")
          "\\)\\(\\|'s\\)\\b"))

(defun highlight-character-names ()
  "Highlight character names in prose with a subtle style."
  (let ((names (get-org-character-names)))
    (when names

  (font-lock-add-keywords
   nil
   `((,(character-names-regex names)
      (1 'font-lock-keyword-face)  ;; Subtle highlighting for names
      (2 'font-lock-keyword-face)  ;; Subtle highlighting for possessive forms
      ))))
    ))

(defun highlight-prose-syntax ()
  "Highlight quotations and character names in prose."
  (font-lock-add-keywords
   nil
   '(
     ;; Highlight quotations in green, including the quotes
     ("\"\\([^\"\n]*\\(?:\n[^\"\n][^\"\n]*\\)*\\)\"\\|\"\\([^\"\n]*\\(?:\n[^\"\n][^\"\n]*\\)*\\)\n\n"
      (0 'font-lock-string-face t))
     ))
  (highlight-character-names)
  (font-lock-update)
  )

(defun get-org-character-names ()
  "Extract character names from the #+CHARACTER_NAMES keyword in the current buffer."
  (let ((case-fold-search t)
        (names nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+CHARACTER_NAMES: \\(.*\\)$" nil t)
        (setq names (split-string (match-string 1) ",\\s-*"))))
    names))
(add-hook 'cf/writing-mode-hook 'highlight-prose-syntax)
