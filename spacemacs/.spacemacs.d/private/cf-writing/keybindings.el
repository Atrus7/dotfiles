(evil-leader/set-key (kbd "o v i") 'cf/vlc-grab-timestamp)
(evil-leader/set-key (kbd "o v v") 'cf/vlc-toggle-pause)
(evil-leader/set-key (kbd "o w d") 'cf/write-today)
(evil-leader/set-key (kbd "x t") 'mw-thesaurus-lookup-dwim)
(evil-leader/set-key (kbd "x d") 'define-word-at-point)
(evil-leader/set-key (kbd "o w n") 'cf/narrow-indirect-buffer)

(evil-leader/set-key (kbd "t w") 'spacemacs/toggle-cf/writing-mode)
;; this will be "editing mode" eventually
(evil-leader/set-key (kbd "t e") 'spacemacs/toggle-cf/editing-mode)


(define-minor-mode books-org-quotes-mode
  "A minor mode for 'books.org' file."
  :init-value nil
  :lighter " Books"
  (if books-org-quotes-mode
      (progn
        (evil-local-set-key 'normal (kbd "C-n") 'insert-my-string)
        (evil-local-set-key 'insert (kbd "C-n") 'insert-my-string)

        ;; This is just an example, adjust the string as needed.
        (defun insert-my-string ()
          "Insert a specified string."
          (interactive)
          (evil-normal-state 1)
          (insert-block-quote)
          (evil-ex-nohighlight)
          (evil-insert 1))
        (message "Activated books-org-quotes-mode"))
    (progn
      ;; Restore the original evil-mode binding for C-n in the current buffer
      (evil-local-set-key 'normal (kbd "C-n") nil)
      (evil-local-set-key 'insert (kbd "C-n") nil)
    (message "Deactivated books-org-quotes-mode"))))


(fset 'insert-block-quote
      (kmacro-lambda-form [?/ ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- ?- return ?y ?y ?o ?# ?+ ?B ?E ?G ?I ?N ?_ ?Q ?U ?O ?T ?E return return ?# ?+ ?E ?N ?D ?_ ?Q ?U ?O ?T ?E ?f ?d ?p up up] 0 "%d"))

(defun books-org-mode-checker ()
  "Turn on `books-org-quotes-mode' when in 'books.org'."
  (when (and (buffer-file-name)
             (string-match "books\\.org$" (buffer-file-name)))
    (books-org-quotes-mode 1)))

(add-hook 'find-file-hook 'books-org-mode-checker)
