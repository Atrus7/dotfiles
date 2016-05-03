;;; Space -- Out of this world
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")
;;; Evil -- We've joined the dark side.
(require 'evil)
(evil-mode 1) ;; this line must be after we set the leader
;; Evil escape??
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
                                        ;"Fd" ;delete frame
                                        ;"Fo" '
  )


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

; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") (lambda() (interactive ) (previous-line) (newline-below-point) ))

(el-init-provide)
