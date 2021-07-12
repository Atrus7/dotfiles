;;; packages.el --- cf-org layer packages file for Spacemacs.

;;  Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(helm
                            flyspell-lazy
                            org
                            evil-org
                            org-agenda)
  "The list of Lisp packages required by the cf-org layer.")

(defun cf-org/post-init-evil-org()
  (use-package evil-org
    :defer t
    :config
    ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
    (evil-org-set-key-theme '(textobjects insert shift todo heading return))
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

    (setq org-agenda-files '("~/org/todo.org" "~/org/books.org"))
    (setq org-agenda-custom-commands cf/custom-agenda)

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

    ;; turn list to checkbox.
    (spacemacs/set-leader-keys-for-major-mode 'org-mode (kbd "]") (lambda () (interactive) (org-toggle-checkbox '(4))))


    (add-to-list 'org-modules 'org-habit t) ;; use org-habit
    (setq org-habit-following-days 1 ;; Just include today
          org-habit-completed-glyph ?✓
          org-habit-show-done-always-green t)

    ;; blog publishing
    (spacemacs/declare-prefix (kbd "o p") "publishing")
    (evil-leader/set-key (kbd "a o p") 'org-publish)
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

;;; packages.el ends here
