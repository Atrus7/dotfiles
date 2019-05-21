;;; packages.el --- cf-org layer packages file for Spacemacs.

;;  Configures the behavior of org mode within emacs

;;; Code:

(defconst cf-org-packages '(helm org org-agenda)
  "The list of Lisp packages required by the cf-org layer.")

(defun cf-org/post-init-org()
  (use-package org
    :defer t
    :config
    (add-to-list 'org-src-lang-modes '("scheme" . scheme) )
    (add-to-list 'org-babel-load-languages '(scheme . t))
    (add-to-list 'org-babel-load-languages '(C . t))
    (add-to-list 'org-babel-load-languages '(C++ . t))
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                             (scheme . t)
                                                             (C . t)
                                                             ))
    (setq org-agenda-files (list(mapconcat 'eval '(org-directory "todo.org") "/")))

    ;; remapping pomodoro thing
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "p" 'org-priority)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "p" 'org-agenda-priority)

    ;; blog publishing
    (spacemacs/declare-prefix (kbd "o p") "publishing")
    (evil-leader/set-key (kbd "a o p") 'org-publish)
    (evil-leader/set-key (kbd "o p a") 'org-publish-all)
    (evil-leader/set-key (kbd "o p f") 'org-publish-current-file)
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
  (use-package helm
    :defer t
    :config

    (put 'helm-ff-run-insert-blog-img 'helm-only t)
    (define-key helm-find-files-map (kbd "C-c s")         'helm-ff-run-insert-blog-img)
    (helm-add-action-to-source "Insert blog image path" 'helm-files-insert-as-static-link helm-source-find-files)
    )
  )

;;; packages.el ends here
