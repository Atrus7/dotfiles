
(setq org-hide-emphasis-markers t
      org-hide-block-startup t
      org-catch-invisible-edits 'smart)

(setq org-log-done 'time
      ;; Book reviews don't really need to track exactly when things get done by the hour...
      org-log-done-with-time nil
      )

;; Don't indent the normal text
(setq org-adapt-indentation nil)

(setq org-refile-use-cache t)

(setq org-enforce-todo-dependencies t)

;; all the capture configuring

;; Prevent org capture from warning in a perspective
(setq persp-kill-foreign-buffer-action nil)

;; For the horrendous Weekly Review string, just evaluate this:
;; (progn
;;   (with-temp-buffer
;;     (insert-file-contents "~/.spacemacs.d/private/templates/weekly_review.org")
;;     (buffer-string)))

(setq org-capture-templates
      `(("T" "Linked-Todo" entry (file+headline ,(cf/get-orgfiles-path "todo.org" )"Tasks")
         "* TODO %?\n %i \n %a\n  Entered on %U")
        ("t" "Todo" entry (file+headline ,(cf/get-orgfiles-path "todo.org" )"Tasks")
         "* TODO %?\n  %i\n  Entered on %U")
        ("j" "Journal" entry (file+datetree ,(cf/get-orgfiles-path  "journal.org"))
         "* %?\nEntered on %U\n  %i\n")
        ("d" "daily" entry (file+datetree ,(cf/get-orgfiles-path  "daily.org"))
         "* TODO %?\n %i\n SCHEDULED: %t ")
        ("i" "Idea" entry (file+headline ,(cf/get-orgfiles-path "ideas.org") "Ideas")
         "* %i%?\n  Noted on %U \n  ")
        ("c" "Code" entry (file+headline ,(cf/get-orgfiles-path "code_snippets.org") "Code Snippets")
         "* Snippet %?\n Entered on %U\n  %i\n  %a")
        ("b" "Book" entry (file+headline ,(cf/get-orgfiles-path "books.org") "Unfiled")
         "** TO_READ %?\n Entered on %U\n  %i\n ")
        ("m" "Misc" entry (file+headline ,(cf/get-orgfiles-path "misc.org") "Misc")
         "* %?\n Entered on %U\n  %i\n ")
        ("M" "Mail" entry (file+headline ,(cf/get-orgfiles-path "todo.org" ) "Tasks")
         "* TODO %? :mail:\n  %i\n %a ")
        ("w" "Weekly Review" entry (file+datetree ,(cf/get-orgfiles-path "weekly_review.org"))
         ,(cf/org-pull-template-from-file  "~/.spacemacs.d/private/templates/weekly_review.org")

)))
(setq org-archive-location "~/org/archive.org::")

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq
 org-agenda-include-diary t
 org-deadline-warning-days 10)

(setq
 org-html-head (cf/org-pull-template-from-file "~/blog/publishing/links.html")
 org-html-preamble      t
 org-html-postamble     t
 org-html-inline-images t
 org-html-preamble-format `(("en"
                             ,(cf/org-pull-template-from-file "~/blog/publishing/header.html"))))

(setq server-blog-base "/ssh:webserver:/var/www/html/syscowboy/posts")
(setq server-static-base "/ssh:webserver:/var/www/html/syscowboy/static")

;; website related
(setq org-publish-project-alist
      `(("blog"
         :components ("blog-content" "blog-static"))
        ("blog-content"
         :base-directory       "~/blog/posts"
         :base-extension       "org"
         :publishing-directory ,server-blog-base
         ;;                    :email cf/personal-email
         :recursive            t
         :publishing-function  org-html-publish-to-html
         :completion-function  cf/pass
         :with-tags            nil
         ;; Just the default for this project.
         :headline-levels      4
         :with-toc             nil
         :with-title           t
         :with-email           nil
         :section-numbers      nil
         :with-sub-superscript nil
         :with-todo-keywords   nil
         ;:author              Chris Findeisen
         :with-creator         nil
         :timestamp            t
         :exclude-tags         ("noexport" "todo")
         :auto-sitemap         t
         :sitemap-sort-folders first
         :sitemap-sort-files   anti-chronologically
         :sitemap-ignore-case  t
         :sitemap-title        "home"
                               )
        ("blog-static"
         :base-directory "~/blog/posts/static"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf\\|ico"
         :publishing-directory ,server-static-base
         :recursive t
         :publishing-function org-publish-attachment)))
