(require 'cf)
(require 'evil)
(require 'evil-leader)
(require 'evil-surround)

;;; Evil Leader. Space -- Out of this world
(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")

;;; Evil -- We've joined the dark side.
(evil-mode 1) ;; this line must be after we set the leader

; Evil escape...
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

(evil-leader/set-key
    ;; Super important one-key spacecuts
  "d" 'dired
  "f" 'helm-find-files
  ;; Buffer stuff
  "bb" 'helm-buffers-list
  "bs" 'switch-to-scratch-and-back
  ;"rb" 'revert-buffer
  ;; Misc
  "gs" 'magit-status
  "gc" 'count-words
  ;; Evaluation
  "eb" 'eval-buffer
  "el" 'eval-expression
  "er" 'eval-region

  ;; Window management
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wd" 'evil-window-delete
  "wj" (lambda() (interactive) ( evil-window-decrease-height 5 ))
  "wk" (lambda() (interactive) ( evil-window-increase-height 5 ))
  "wh" (lambda() (interactive) ( evil-window-decrease-width 5 ))
  "wl" (lambda() (interactive) ( evil-window-increase-width 5 ))
  "wu" 'winner-undo
  "wr" 'winner-redo

  ;; Frame managmenent
  "Fn" 'make-frame-command
  "Fd" 'delete-frame
  "Fo" 'other-frame

  "Fs" 'window-configuration-to-register ; Save Frame with buffer and layouts( not to disk...yet )
  "Fl" 'jump-to-register  ; load Frame with buffers and layouts

  ;; Relative Line numbers
  "r" (lambda() (interactive) (linum-relative-toggle))

  ;; Theme stuff
  "tl" (lambda() (interactive) (load-theme 'solarized-light 'NO-CONFIRM))
  "td" (lambda() (interactive) (load-theme 'gruvbox 'NO-CONFIRM))
  "sc" 'yas-expand
  ;"sr" 'helm-yas-create-snippet-on-region
;"sf" 'helm-yas-visit-snippet-file
  )


;; Window movement mappings. Global like the Wolf
(dolist (key '("\M-h" "\M-j" "\M-k" "\M-l"))
  (global-unset-key key))


;; Mappings
(define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)

(define-key evil-motion-state-map (kbd "M-e") 'eval-buffer)
(define-key evil-motion-state-map (kbd "M-;") 'cf/append-semicolon)

; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") (newline-above-point))

; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)

;; Evil-surround Get surround keys  working
(global-evil-surround-mode 1)

(defun cf/move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;; creates a newline without breaking the current line
(defun newline-below-point ()
  "1. Move to end of line
   2. insert newline with indentation"
  (interactive)
  (let((oldpos(point)))
    (end-of-line)
    (newline-and-indent)))


(defun newline-above-point ()
  (interactive)
  (if (eq (line-number-at-pos) 1)
      (progn (beginning-of-line) (newline) (previous-line))
    (progn (previous-line) (newline-below-point))
    )
  )

(el-init-provide)
