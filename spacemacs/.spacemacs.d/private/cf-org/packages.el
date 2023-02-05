;;; packages.el --- cf-org layer packages file for Spacemacs.

;;  Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(helm
                            flyspell-lazy
                            org
                            evil-org
                            org-wc
                            org-agenda
                            org-fancy-priorities)
  "The list of Lisp packages required by the cf-org layer.")

(defun cf-org/post-init-evil-org()
  (use-package evil-org
    :defer t
    :config
    ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
    (evil-org-set-key-theme '(textobjects insert todo heading return))
    ))

(defun cf-org/post-init-org()
  (use-package org
    :defer t
    :config
    (add-to-list 'org-src-lang-modes '("scheme" . scheme) )
    (add-to-list 'org-babel-load-languages '(scheme . t))
    (add-to-list 'org-babel-load-languages '(shell . t))
    (add-to-list 'org-babel-load-languages '(C . t))
    (add-to-list 'org-babel-load-languages '(C++ . t))
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                             (scheme . t)
                                                             (shell . t)
                                                             (C . t)
                                                             ))

    ;; Headline "ignoring" via ox-extra
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    ;; Adds support for multiple tags.
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))

    ;; Standard todos
    (defvar cf/todo-files '("~/org/todo.org"))
    ;; Add the novel todos
    (if (file-exists-p (concat current-novel-path "todo.org"))
        (add-to-list 'cf/todo-files (concat current-novel-path "todo.org")))

    (defvar cf/book-files '("~/org/books.org"))
    (setq org-agenda-files (append cf/todo-files cf/book-files))

    (setq cf/custom-agenda
          '(
            ("l" "Agenda and all TODOs"
             ((agenda #1="")
              (alltodo #1#)))
            ("n" "Novel Work"
             ((todo "TODO" ((org-agenda-files (list current-novel-path))
                            (org-agenda-overriding-header "Novel Todos:")))))
            ("b" "Books" todo "TO_READ|READING" ((org-agenda-files cf/book-files)))
            ("c" "Christopher's Agenda"
             (
              (agenda "" ((org-agenda-span 'day)
                          (org-deadline-warning-days 0)
                          (org-scheduled-past-days 1)
                          (org-deadline-past-days 1)
                          (org-agenda-sorting-strategy '(scheduled-up deadline-up))
                          (org-agenda-overriding-header "TODAY:")
                          (org-agenda-format-date "")
                          ))
              (tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "HIGH PRIORITY TASKS: ")))
              (agenda ""
                      (
                       (org-agenda-span 'day)
                       (org-agenda-start-day "+1d")
                       (org-deadline-warning-days 0)
                       (org-scheduled-past-days 0)
                       (org-deadline-past-days 0)
                       (org-habit-show-habits-only-for-today nil)
                       (org-agenda-sorting-strategy '(scheduled-up deadline-up))
                       (org-agenda-overriding-header "TOMORROW:")
                       (org-agenda-format-date "")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       ))
              (agenda "" ((org-agenda-span 'day)
                          (org-deadline-warning-days 0)
                          (org-agenda-sorting-strategy '(deadline-up))
                          (org-agenda-skip-function 'cf/skip-entry-unless-overdue-deadline)
                          (org-agenda-overriding-header "OVERDUE:")
                          (org-agenda-format-date "")
                          ))

              (todo "TODO" ((org-agenda-files cf/todo-files)
                            (org-agenda-overriding-header "TODO LIST:")
                            ;; (org-agenda-sorting-strategy '(scheduled-up deadline-up))
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                            ))
              (todo "TO_READ|READING" ((org-agenda-files cf/book-files)
                                       (org-agenda-overriding-header "BOOK LIST:")
                                       ))
              )))
          )

    (setq org-agenda-custom-commands cf/custom-agenda)

    (setq org-persp-startup-with-agenda "c")

    ;; Set the DONE font.
    ;; In many of my org files, I don't want to track DONE lines with strikethrough, for example books.org
    (set-face-attribute 'org-headline-done nil
                        :foreground "slate grey"
                        :strike-through nil)

    ;; remapping pomodoro thing
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "p" 'org-priority)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "p" 'org-agenda-priority)

    ;; better pdf exporting
    (setq revert-without-query '(".pdf"))
    (evil-define-key '(normal insert visual) org-mode-map (kbd "<f5>")
      (lambda() (interactive) (find-file-other-window
                               (org-latex-export-to-pdf))

        (pdf-view-goto-page 1)
        )

      )
    ;; Somehow org-mode messes this up, making it Org-shift-down. Explicitly remap it
    (evil-define-key '(normal) org-mode-map (kbd "J") 'evil-join)

    ;; turn list to checkbox.
    (spacemacs/set-leader-keys-for-major-mode 'org-mode (kbd "]") (lambda () (interactive) (org-toggle-checkbox '(4))))


    (add-to-list 'org-modules 'org-habit t) ;; use org-habit
    (setq org-habit-following-days 1 ;; Just include today
          org-habit-completed-glyph ?✓
          org-habit-show-done-always-green t)

    ;; blog publishing
    (spacemacs/declare-prefix (kbd "o p") "publishing")
    (evil-leader/set-key (kbd "o p p") 'org-publish)
    (evil-leader/set-key (kbd "o p a") 'org-publish-all)
    (evil-leader/set-key (kbd "o p f") 'cf/org-publish-file)
    ;; remapping over table
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "t" 'cf/org-schedule-today)

    (spacemacs/set-leader-keys-for-major-mode 'org-mode "h" 'cf/highlight-region)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "H" 'cf/unhighlight-region)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "I" 'org-toggle-inline-images)


    (evil-define-key '(normal) evil-org-mode-map
      (kbd "<S-return>") 'org-insert-subheading
      (kbd "M-<RET>") 'org-insert-heading
      (kbd "C-<RET>") 'org-insert-heading
      )

    (evil-define-key '(normal insert) evil-org-mode-map
      (kbd "<right>") 'org-shiftmetadown
      (kbd "<left>") 'org-shiftmetaup
      (kbd "<up>") 'org-metaup
      (kbd "<down>") 'org-metadown

      (kbd "M-h") 'evil-window-left
      (kbd "M-j") 'evil-window-down
      (kbd "M-k") 'evil-window-up
      (kbd "M-l") 'evil-window-right)
    )
  )

(defun cf-org/post-init-org-agenda ()
  (use-package org-agenda
    :defer t
    :config
    ;; Un-evilify the window management.
    (evil-define-key 'evilified org-agenda-mode-map (kbd "M-h") nil)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "M-j") nil)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "M-k") nil)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "M-l") nil)
    (evil-define-key 'evilified org-agenda-mode-map (kbd "M-d") nil)
    )
  )

(defun cf-org/post-init-helm()
  (require 'helm-files)
  (put 'helm-ff-run-insert-blog-img 'helm-only t)
  (define-key helm-find-files-map (kbd "C-c s")         'helm-ff-run-insert-blog-img)
  (customize-set-variable 'helm-ff-lynx-style-map t)

  ;; needs to be a hook
  ;; https://github.com/emacs-helm/helm/issues/604
  (add-hook 'helm-find-files-before-init-hook
            (lambda ()
              (helm-add-action-to-source "Insert blog image path" 'helm-files-insert-as-static-link helm-source-find-files)
              )
            )

  )
(defun cf-org/init-flyspell-lazy()
  (use-package flyspell-lazy
    :config
    (flyspell-lazy-mode 1)
    )
  )

(defun cf-org/init-org-wc()
  (use-package org-wc
    :config
    )
  )

(defun cf-org/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :ensure t
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-priority-highest 0
          org-priority-default 2
          org-priority-lowest 4)
    (setq org-fancy-priorities-list '(
                                      (?0 . "P0")
                                      (?1 . "P1")
                                      (?2 . "P2")
                                      (?3 . "P3")
                                      (?4 . "P4"))

          org-priority-faces '((?0 :foreground "DarkRed" :background "LightPink")
                               (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
                               (?2 :foreground "gray20" :background "gray")
                               (?3 :foreground "gray20" :background "gray")
                               (?4 :foreground "gray20" :background "gray")
)


    ))

  )


;;; packages.el ends here
