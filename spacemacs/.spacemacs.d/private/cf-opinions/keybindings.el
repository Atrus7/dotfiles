;; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)
;; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") 'newline-above-point)

(spacemacs/set-leader-keys (kbd "x i") 'insert-char)

(spacemacs/set-leader-keys (kbd "b r") 'rename-buffer)

;; Quick way to get a calculation in the buffer, SPC #
(spacemacs/set-leader-keys (kbd "#") (lambda () (interactive)
  (quick-calc t)))

;; Make macros handy
(evil-global-set-key 'normal (kbd "Q") 'call-last-kbd-macro)
(evil-global-set-key 'normal (kbd "C-q") 'name-last-kbd-macro)

;; win movement
(define-key global-map (kbd "M-h") #'evil-window-left)
(define-key global-map (kbd "M-j") #'evil-window-down)
(define-key global-map (kbd "M-k") #'evil-window-up)
(define-key global-map (kbd "M-l") #'evil-window-right)
(define-key global-map (kbd "M-d") #'spacemacs/delete-window)

;; e in dired to edit
(evil-define-key '(normal)
  dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)

;; fix search in dired
(evil-define-key 'normal dired-mode-map
  (kbd "n") 'evil-ex-search-next)
(evil-define-key 'normal dired-mode-map
  (kbd "N") 'evil-ex-search-previous)


(evil-define-key 'normal 'global
  (kbd "M-h") #'evil-window-left
  (kbd "M-j") #'evil-window-down
  (kbd "M-k") #'evil-window-up
  (kbd "M-l") #'evil-window-right
  (kbd "M-d") #'spacemacs/delete-window

  (kbd "M-Q") #'cf/unfill-paragraph
  (kbd "M-=") #'indent-buffer
  )

(define-key evil-evilified-state-map (kbd "M-h") #'evil-window-left)
(define-key evil-evilified-state-map (kbd "M-j") #'evil-window-down)
(define-key evil-evilified-state-map (kbd "M-k") #'evil-window-up)
(define-key evil-evilified-state-map (kbd "M-l") #'evil-window-right)
(define-key evil-evilified-state-map (kbd "M-d") #'spacemacs/delete-window)

;; So SPC f R is renaming a file.
;; So SPC f D is deleting a file.
;; SPC f C should be copying a file.
(evil-leader/set-key (kbd "f C") 'spacemacs/copy-file)

(evil-define-key 'normal prog-mode-map
  (kbd "C-;") 'append-semicolon)

(evil-define-key 'insert 'global (kbd "M-RET") #'newline-below-point)

;; To search in the buffer, SPC s S is still available. This is way faster for
;; large files and mostly what I want
(evil-leader/set-key (kbd "s s") 'spacemacs/helm-file-do-ag-region-or-symbol)

(evil-leader/set-key (kbd "g b") 'magit-blame)
(evil-leader/set-key (kbd "g f f") 'magit-blob-visit-file)

;; Macros stored here
(fset 'exchange-words "gxewgxe")
(fset 'remove-surrounding-function
      "f)ds)dB")

(fset 'swap-around-hyphen
      "0WWvt-begxf-wv$hgx")

(fset 'gerrit-link-to-depends-on
      [?0 ?/ ?e ?u ?r return ?d ?B ?/ ?r ?e ?v ?i ?e ?w return ?h ?m ?m ?$ ?F ?/ ?d ?` ?m ?I ?D ?e ?p ?e ?n ?d ?s ?- ?O ?n ?: ?  escape ?0])


(evil-global-set-key 'normal (kbd "gw") 'exchange-words)

(spacemacs/set-leader-keys (kbd "b !") 'reload-buffer)
(spacemacs/set-leader-keys (kbd "f y") 'cf/abbreviate-show-and-copy-filename)
(spacemacs/set-leader-keys (kbd "f Y") 'spacemacs/show-and-copy-buffer-filename)

;; Store macro-based ops
(spacemacs/declare-prefix (kbd "o") "cf/")
(spacemacs/declare-prefix (kbd "o m") "Macros")
(spacemacs/declare-prefix (kbd "o w") "Writing")
(spacemacs/declare-prefix (kbd "o b") "Buffers")
(spacemacs/declare-prefix (kbd "o s") "Search")
(spacemacs/declare-prefix (kbd "o t") "Tramp")
;; TODO map describe last-function
;; run current function


(evil-leader/set-key (kbd "fep") 'cf/find-private-layers)
;; no need for the dotfile-diff..use d for downloads
(evil-leader/set-key (kbd "fed") 'cf/find-downloads)
(evil-leader/set-key (kbd "feD") 'spacemacs/find-dotfile)
(evil-leader/set-key (kbd "feo") 'cf/find-org-files)
(evil-leader/set-key (kbd "p s") 'cf/projectile-magit)
(evil-leader/set-key (kbd "p p") 'cf/projectile-default-switch)
(evil-leader/set-key (kbd "p /") 'cf/projectile-search)

(evil-leader/set-key (kbd "o s d") 'cf/hidden-project-ag)

(evil-leader/set-key (kbd "o m f") 'remove-surrounding-function)
(evil-leader/set-key (kbd "o b s") 'cf/save-scratch-and-file)
(define-key evil-normal-state-map (kbd "s") 'remove-surrounding-function)

(evil-leader/set-key (kbd "S l") 'cf/learn-word)
(evil-leader/set-key (kbd "S A") 'cf/flyspell-and-save-abbrev)

(evil-leader/set-key "owl" 'cf/chrome-linux-ident)
(evil-leader/set-key "owg" 'cf/chrome-linux-ident)
(evil-global-set-key 'normal "s" 'evil-exchange)

(evil-leader/set-key "is" 'yas-insert-snippet)
(evil-global-set-key 'normal (kbd "C-M-;") 'append-semicolon)

;; (evil-leader/set-key "ww" 'ace-select-window)

;; TODO this doesn't work yet
;; (evil-global-set-key 'visual "S" 'evil-exchange)

;; in visual mode, automatically default to surround characters
(evil-global-set-key 'visual "\"" '(lambda () (interactive) (insert-pair nil ?\" ?\")) )
(evil-global-set-key 'visual "\'" '(lambda () (interactive) (insert-pair nil ?\' ?\')) )
