;; org-capture

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Ideas" entry (file+headline "~/org/ideas.org" "Ideas")
         "* %i%?\n  Noted on %U \n  ")
        ("c" "Code" entry (file+headline "~/org/code_snippets.org" "Code Snippets")
         "* Snippet %?\n Entered on %U\n  %i\n  %a")
        ))

(global-set-key (kbd "C-c c") 'org-capture)
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(el-init-provide)
