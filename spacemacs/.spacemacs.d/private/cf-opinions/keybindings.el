; The cousin of J
(define-key evil-normal-state-map "S" 'electric-newline-and-maybe-indent)
; the normal mode cousins of o and C-o.
(define-key evil-normal-state-map (kbd "RET") 'newline-below-point)
(define-key evil-normal-state-map (kbd "<C-return>") 'newline-above-point)

; Control s to save
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-visual-state-map (kbd "C-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "C-s") 'save-buffer)

;; Make macros handy
(evil-global-set-key 'normal (kbd "Q") 'call-last-kbd-macro)
(evil-global-set-key 'normal (kbd "C-q") 'name-last-kbd-macro)

;; Macros stored here
(fset 'exchange-words "gxewgxe")
(fset 'remove-surrounding-function
   "f)ds)dB")

(fset 'swap-around-hyphen
   "0WWvt-begxf-wv$hgx")

(fset 'gerrit-link-to-depends-on
   [?/ ?e ?u ?r return ?d ?B ?/ ?r ?e ?v ?i ?e ?w return ?h ?m ?m ?$ ?F ?/ ?d ?` ?m ?I ?D ?e ?p ?e ?n ?d ?s ?- ?O ?n ?: ?  escape ?0])


(evil-global-set-key 'normal (kbd "gw") 'exchange-words)


;; Store macro-based ops
(spacemacs/declare-prefix (kbd "o") "cf/")
(spacemacs/declare-prefix (kbd "o m") "Macros")
(spacemacs/declare-prefix (kbd "o b") "Buffers")
(spacemacs/declare-prefix (kbd "o t") "Tramp")

(evil-leader/set-key (kbd "o m f") 'remove-surrounding-function)
(evil-leader/set-key (kbd "o b s") 'cf/save-scratch-and-file)
(evil-leader/set-key (kbd "o b s") 'cf/save-scratch-and-file)
;; (define-key evil-normal-state-map (kbd "s") 'remove-surrounding-function)


(evil-leader/set-key "owl" 'cf/chrome-linux-ident)
(evil-global-set-key 'normal "s" 'evil-exchange)

;; TODO this doesn't work yet
;; (evil-global-set-key 'visual "S" 'evil-exchange)
