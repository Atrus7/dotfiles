;; all the capture configuring

;; For the horrendous Weekly Review string, just evaluate this:
;; (progn
;;   (with-temp-buffer
;;     (insert-file-contents "~/.spacemacs.d/private/templates/weekly_review.org")
;;     (buffer-string)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  Entered on %U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* %i%?\n  Noted on %U \n  ")
        ("c" "Code" entry (file+headline "~/org/code_snippets.org" "Code Snippets")
         "* Snippet %?\n Entered on %U\n  %i\n  %a")
        ("b" "Book" entry (file+headline "~/org/books.org" "Unfiled")
         "** TO_READ %?\n Entered on %U\n  %i\n ")
        ("m" "Misc" entry (file+headline "~/org/misc.org" "Misc")
         "* %?\n Entered on %U\n  %i\n ")
        ("w" "Weekly Review" entry (file+datetree "~/org/weekly_review.org" "Weekly Review")
         "* Reflection on System %?\n** What went well?\n** What should be adjusted?\n\n* Brain Dump [0%]\n- [ ] School\n- [ ] Email Items\n- [ ] Appointments or Waiting-ons\n- [ ] Kailah, romance, gifts, anniversary\n- [ ] Recommendations\n  - Books\n  - Movies\n  - Food\n  - Amazon\n\n* Reflection on Habits\n** Success Indicators [/]\n  Check the ones that I rocked this week!\n    - [ ] Reading\n    - [ ] Working Out\n    - [ ] NF\n    - [ ] Mindfulness\n    - [ ] Diet\n** Thoughts\n"
)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)
;(add-hook 'org-mode-hook )

(setq
 org-agenda-include-diary t
 org-deadline-warning-days 10)

;; to be run after org is loaded
(spacemacs|use-package-add-hook org
  :post-config
  (add-to-list 'org-src-lang-modes '("scheme" . scheme) )
  (add-to-list 'org-babel-load-languages '(scheme . t))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (scheme . t) ))
  (setq org-agenda-files (list(mapconcat 'eval '(org-directory "todo.org") "/")))
  )
