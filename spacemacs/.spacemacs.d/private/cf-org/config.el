(setq org-hide-emphasis-markers t
      org-hide-block-startup t ;; close blocks on startup
      org-startup-folded 'content ;; Just show headlines
      org-catch-invisible-edits 'smart)

(setq org-log-done 'time
      ;; Book reviews don't really need to track exactly when things get done by the hour...
      org-log-done-with-time nil)

;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "docx")


;; https://github.com/politza/pdf-tools
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(setq-default pdf-view-display-size 'fit-page)

;; Want to insert the word-count when clocking out.
(setq org-log-note-clock-out t)
(add-hook 'org-log-buffer-setup-hook
          'cf/org-clock-out-log)

;; Don't indent the normal text
(setq org-adapt-indentation nil)


(setq org-refile-use-cache nil ;; this is annoying after a while, when headers change
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-goto-interface 'outline-path-completion
      org-link-file-path-type 'absolute)  ;; need this in case of files moving, relative links break everything.

(setq org-enforce-todo-dependencies t)

;; all the capture configuring

;; Prevent org capture from warning in a perspective
(setq persp-kill-foreign-buffer-action nil)

;; For the horrendous Weekly Review string, just evaluate this:
;; (progn
;;   (with-temp-buffer
;;     (insert-file-contents "~/.spacemacs.d/private/templates/weekly_review.org")
;;     (buffer-string)))

(setq org-directory "~/org")

;; symlink this to real path.
(defvar current-novel-path (file-truename "~/writing/projects/current-novel/"))

(defun cf/find-novel ()
  "shortcut to private layers dir"
  (interactive)
  (helm-find-files-1 current-novel-path))

(setq org-capture-templates
      `(("T" "Linked-Todo" entry (file+headline ,(cf/get-orgfiles-path "todo.org" )"Tasks")
         "* TODO %? %a\n %i \n")
        ("t" "Todo" entry (file+headline ,(cf/get-orgfiles-path "todo.org" )"Tasks")
         "* TODO %?\n  %i\n  Entered on %U")
        ("j" "Journal" entry (file+datetree ,(cf/get-orgfiles-path  "journal.org"))
         "* %?\nEntered on %U\n  %i\n")
        ("i" "Idea" entry (file+headline ,(cf/get-orgfiles-path "ideas.org") "Ideas")
         "* %i%?\n  Noted on %U \n  ")
        ("c" "Code" entry (file+headline ,(cf/get-orgfiles-path "code_snippets.org") "Code Snippets")
         "* Snippet %?\n Entered on %U\n  %i\n  %a")
        ("b" "Book" entry (file+headline ,(cf/get-orgfiles-path "books.org") "Unfiled")
         "** TO_READ %?\n Entered on %U\n  %i\n ")
        ("m" "Misc" entry (file+headline ,(cf/get-orgfiles-path "misc.org") "Misc")
         "* %?\n Entered on %U\n  %i\n ")
        ("q" "Quote" entry (file+headline ,(cf/get-orgfiles-path "quote.org") "Quote")
         "* %? \n%i\n ")
        ("M" "Mail" entry (file+headline ,(cf/get-orgfiles-path "todo.org" ) "Tasks")
         "* TODO %? :mail:\n  %i\n %a")

        ;; ("w" "writing" entry (file+headline ,("todo.org")) "Tasks"
        ;;  "* TODO %?\n %a\n ")

        ;; identical to linked todo
        ;; This will default to org-directory. Which we will set in writing projects.
        ("n" "Novel Todo" entry (file+headline ,(concat current-novel-path "todo.org") "Tasks")
         "* TODO %? %a\n %i \n")
        ))

;; relative file links should work.
(setq org-link-search-must-match-exact-headline nil)



(setq org-archive-location "~/org/archive.org::")

(add-hook 'org-capture-mode-hook 'evil-insert-state)


(setq holiday-local-holidays
      '((holiday-fixed 2 29 "Wedding Anniversary")
        (holiday-fixed 3 19 "C- Birthday")
        (holiday-fixed 4 11 "Dating Anniversary")
        (holiday-fixed 2 7 "R- Birthday")
        (holiday-fixed 10 31 "A- Birthday")
        (holiday-fixed 2 23 "K- Birthday")
        (holiday-fixed 9 10 "B- Birthday")))

(setq
 org-agenda-include-diary t
 org-deadline-warning-days 10
 calendar-holidays (append holiday-general-holidays holiday-local-holidays
                           holiday-christian-holidays holiday-solar-holidays))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up category-keep priority-down)
        (todo   category-keep priority-down)
        (tags   priority-down category-keep)
        (search category-keep)))

(setq
 org-export-with-email nil
 org-export-with-toc nil
 org-export-with-section-numbers nil
 org-export-with-sub-superscripts nil
 org-html-preamble      t
 org-html-postamble     t
 org-html-postamble-format `(("en" ,(cf/org-pull-template-from-file "~/blog/publishing/footer.html")))
 org-html-inline-images t
 org-html-preamble-format `(("en"
                             ,(cf/org-pull-template-from-file "~/blog/publishing/header.html")))
 org-html-head (cf/org-pull-template-from-file "~/blog/publishing/links.html"))

;; Cleanup tex files after pdf export
;; (add-to-list 'org-latex-logfiles-extensions "tex")

;; To keep all logfiles run this:
;; (setq org-latex-logfiles-extensions '()) ;"bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv"))

(setq server-blog-base "/ssh:webserver:/var/www/html/syscowboy/posts")
(setq server-static-base "/rsync:webserver:/var/www/html/syscowboy/static")

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
         ;:author              Christopher Fin
         :with-creator         nil
         :timestamp            t
         :exclude-tags         ("noexport" "todo")
         :auto-sitemap         t
         :sitemap-sort-folders first
         :sitemap-sort-files   anti-chronologically
         :sitemap-format-entry cf/org-publish-entry-custom
         :sitemap-style        tree
         :sitemap-function     cf/org-publish-sitemap-custom
         :sitemap-ignore-case  t
         :sitemap-title        "Christopher Fin | Blog"
         )
        ("blog-static"
         :base-directory "~/blog/posts/static"
         :base-extension "css\\|js\\|png\\|mp4\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf\\|ico"
         :publishing-directory ,server-static-base
         :recursive t
         :publishing-function org-publish-attachment)))

(with-eval-after-load 'ox-latex
;; For book publishing, we want the latex class to have no "parts". By default, org exports the top-level headings as Parts, second level as Chapters.
  (add-to-list 'org-latex-classes
             '("book-noparts"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

             )
  (add-to-list 'org-latex-classes
               '("screenplay"
                 "\\documentclass{screenplay}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\intslug{%s}" . "\\intslug{%s}")
                 ("\\begin{dialogue}{%s}"  "\\end{dialogue}"  "\\begin{dialogue}{%s}" "\\end{dialogue}" )
                 ))

)
