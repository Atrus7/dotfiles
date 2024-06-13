(setq chris-compile-command "make -j80 otapackage | grep -v \"^\\[\\|^Copy\\|^target\\|^Install\"")

(defun cf/setup-lunch-env ()
  (setq cf/default-env
        (list
         "USE_RBE=1"
         "HOST_BUILD_TYPE=release"
         (concat "TMPDIR=" (expand-file-name "~/tmp/tmp"))
         ))
  )

(defun cf/git-cf-format ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (magit-git-command "git format")))


(defun cf/goto-build-file ()
  (interactive)
  (if (file-exists-p (concat default-directory  "Android.mk"))
      (find-file (concat default-directory "Android.mk"))
    (if (file-exists-p (concat default-directory  "Android.bp"))
        (find-file (concat default-directory "Android.bp"))
      (if (file-exists-p (concat default-directory  "BUILD.gn"))
          (find-file (concat default-directory "BUILD.gn"))
        (message "No Build files found...")
        ))))

(defun cf/find-in-out (pattern)
  (interactive "sRegex: ")
  (find-name-dired (concat mysource "out/target/product/.") pattern))

(defun cf/find-in-chromium-out (pattern)
  (interactive "sRegex: ")
  (let* ((out-dir (concat mysource "chromium/src/"))
         (full-out-path (concat out-dir cf/compile-product "/.")))

    (find-name-dired full-out-path pattern)))

(defvar cf/kernel-path (expand-file-name "...kernel/"))
(defvar mysource
  (expand-file-name
   (if (spacemacs/system-is-linux) "~/code/mysource/"
     (cf/get-desk-path "~/code/mysource/")
     ))
  )



(defun cf/ota-action (&optional btn)
  (interactive)
  ;; (message)
  (let* ((bds (bounds-of-thing-at-point 'filename))
         (start (car bds))
         (end (cdr bds))
         (ota-full-path (buffer-substring-no-properties start end))
         )
    (cf/adb-synchronous-command "kill-server" "")
    (cf/adb-synchronous-command "connect" cf-ip)
    (message (concat "Pushing OTA " ota-full-path))
    (start-file-process
     "OTA PUSH"
     "*ota-pushing-logs*" ;; no buffer because push-ota is verbose...
     (concat mysource "push-ota.sh")
     ota-full-path
     chris-ip)
    ))

(defun cf/adb-synchronous-command (command args)
  (if (not (buffer-live-p adb-buffer))
      (setq adb-buffer (get-buffer-create "*ADB*")))
  (pop-to-buffer adb-buffer nil)
  (end-of-buffer)
  (shell-command (concat "adb " command " " args) 'true))

(defun cf/make-otas-actionable ()
  (interactive)
  (button-lock-mode 1)
  (button-lock-set-button
   "ota.+\\.zip"
   'cf/ota-action
   :face 'link
   :face-policy 'prepend
   :keyboard-binding "RET"
   :help-echo "Push the OTA to device"))

(defun cf/compile ()
  (interactive)
  (let ((default-directory source-root)
        )
    (setq compilation-finish-functions '(cf/push_bin))
    (call-interactively 'compile)

    ;; Projectile fucks up compilation-find file, causing error lookups to never terminate.
    ;; Remove their advice fn to fix this
    (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
    )
  ;;TODO integrate w/ide mode?
  )

(defun cf/push_bin (buffer result)
  (shell-command (concat "adb push " source-root "file")))

(defun cf/ninja-process-db (buffer result)
  ;; (if (string-match "make-.*-eng$" (buffer-name buffer)
  ;;                   )
  (if (string-match "^finished" result)
      (save-current-buffer
        (set-buffer (get-buffer-create "*ninja-compdb*"))
        (erase-buffer) ;; We're gonna use this buffer as staging for lunch-env results

        (let* ((chromium-src (concat source-root "chromium/src/"))
               (chromium-out (concat chromium-src "out_chromium_" cf/compile-product "/release"))
               )

          (shell-command (concat "ninja -C "
                                 chromium-out " -t compdb cxx cc > " chromium-src "compile_commands.json"))
          )
        )
    )
  )



(defun cf/compilation-buffer-naming (mjr-mode)
  (message (concat "*make-" cf/compile-product "-" cf/compile-variant "*")))

(defvar lunch-helper-script
  (if (spacemacs/system-is-linux)
      (expand-file-name "~/bin/emacs-lunch.sh")
    "~/bin/emacs-lunch.sh"))

(defun cf/helm-lunch ()
  (interactive)
  (if (null lunch-targets-cached)
      (cf/load-lunch-targets))
  (helm :sources (helm-build-sync-source "lunch"
                   :action 'cf/lunch-pick
                   :volatile nil
                   :fuzzy-match t
                   :candidates lunch-targets-cached)
        :buffer "*helm async source*"))

;; list of lunch targets...
(defvar lunch-targets-cached nil)

(defun cf/lunch-pick (candidate)
  (interactive)
  (save-current-buffer
    (set-buffer (get-buffer-create "*lunch-pick*"))
    (erase-buffer) ;; We're gonna use this buffer as staging for lunch-env results
    (process-file "/bin/bash" nil "*lunch-pick*" nil lunch-helper-script mysource candidate)
    ;; list of lunch vars as strings
    (cf/set-compilation-vars candidate (get-lines-from-buffer "*lunch-pick*"))
    ))

(defun cf/load-lunch-targets ()
  (interactive)
  (save-current-buffer
    (set-buffer (get-buffer-create "*lunch*"))
    (erase-buffer)
    (process-file "/bin/bash" nil "*lunch*" nil lunch-helper-script mysource )
    (setq lunch-targets-cached (get-lines-from-buffer "*lunch*"))))

(defun get-lines-from-buffer (buffer-name)
  "Returns a list of lines as strings, omitting empty strings."
  (cl-remove-if 'string-empty-p
                (split-string (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)) "\n")))

(defun cf/set-compilation-vars (candidate lunch-results)
  (message (format "Setting lunch vars for %s" candidate))
  (let* ((props (split-string candidate "-"))
         (product (car props))
         (variant (cadr props)))

    (cf/set-source-root)
    (cf/setup-lunch-env) ;; source root should be set
    (setq
     cf/compile-product product
     cf/compile-variant variant

     compilation-environment (append cf/default-env lunch-results
                                     (list
                                      (concat "ANDROID_PRODUCT_OUT=" source-root "out/target/product/" product)
                                      (concat "OUT=" source-root "out/target/product/" product )
                                      )))))

(defun cf/get-cs-path ()
  "Copy filename relative cs...This is useful pasting short links to a file in, but it doesn't do any checks if the file exists."
  (interactive)

  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (let ((link (concat (replace-regexp-in-string
                             mysource
                             "http://src/it/"
                             file-name) "?l=" (number-to-string (line-number-at-pos)))))
          (message (kill-new link)))
      (error "Buffer not visiting a file"))))



(defun get-repo-name()
  "Get current projects path relative to .repo root"
  (interactive)
  (replace-regexp-in-string
   (or (projectile-root-top-down-recurring default-directory '(".repo")) "")
   ""
   (projectile-project-root)
   )
  )

(defun cf/get-gerrit-path (gerrit-link)
  "Copy gerrit path of current file.."
  (interactive)
  (let* ((branch "master") ;; (or magit-get-current-branch)
         (file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new (concat
                            gerrit-link (get-repo-name) "+/refs/heads/" branch "/"
                            (file-relative-name file-name (projectile-project-root))
                            (if (buffer-file-name)
                                (concat "#" (number-to-string (line-number-at-pos)))
                              )

                            )))

      (error "Buffer not visiting a file"))))

(defun cf/custom-search-path ()
  (if default-directory
      (setq compilation-search-path '(nil))
    (setq compilation-search-path
          (list
           (concat source-root "chromium/src/")
           source-root)  ;; should be last because it's not a git dir
          )))

(defun cf/set-source-root ()
  (interactive)
  "Return repo root or nil"
  (let ((root (projectile-root-top-down-recurring default-directory '(".repo"))))
    (if (not (null root))
        (progn
          (if (cl-search chris-root-dirname root)
              (setq mysource root)
            )
          (setq source-root root)
          (message "repo root is %s" root)))))

(defun format-with-source-root (str)
  "expects '%s'"
  (if source-root
      (progn
        (replace-regexp-in-string "%s" (string-trim-right source-root "/") str))
    (error "no repo root")
    )
  )

(advice-add 'projectile-switch-project :after  #'cf/set-source-root)


(defun cf/get-region-or-symbol ()
  ;;; Get the currently highlighted region or the symbol at point
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end) )
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds)))
      (if (null start)
          ""
        (buffer-substring-no-properties start end)))))

;; TODO add go/chris-build-query for sha revisions
(defun cf/chrome--source-ident ()
  (interactive)
  (let ((url (format
              "https://gerritsearch/?q=%s+package:%%5Eit$&type=cs"

              (cf/get-region-or-symbol))))
    (browse-url-chrome url)))


(defun cf/comint-get-output (command)
  (comint-goto-process-mark)
  (insert command)
  (comint-send-input)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line 0)
    (buffer-substring comint-last-input-end (point))))


(defun cf/get-cf-build-name()
  (let ((prop (cf/comint-get-output "getprop | grep ro.build.product"))
        (ignore "[ro.build.product] |"))
    (if (> (length prop) (length ignore))
        (substring prop (length) nil)
      ""
      )
    ))

(defun cf/rename-comint (name)
  (if (> (length name) 4)
      (rename-buffer name t)
    (message "Bad name for comint")))


(defun cf/cf-buffer-name ()
  (interactive)
  (cf/rename-comint
   (cf/get-cf-build-name)))

(defun set-magit-gerrit-remote (remote-name)
  (message "Set gerrit remote to %s" remote-name)
  (setq-local magit-gerrit-remote remote-name)
  )

(defun cf/set-gerrit-remote ()
  (interactive)
  "helm selector for setting magit-gerrit-remote"
  (let ((remotes (magit-list-remotes)))
    (if (= (length remotes) 1)
        ;; just pick it if it's the only one...
        (set-magit-gerrit-remote (car remotes))
      (helm :sources (helm-build-sync-source "gerrit-git-remote"
                       :action 'set-magit-gerrit-remote
                       :volatile nil
                       :fuzzy-match t
                       :candidates remotes)
            :buffer "*helm async source*"))
    ))

(defun cf/magit-push-to-gerrit ()
  "Taken from https://github.com/terranpro/magit-gerrit/blob/master/magit-gerrit.el"
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     "HEAD"))

         (branch-remote (and branch (magit-get "branch" branch "remote")))
         (branch-merge (if (or (null branch-remote)
                               (string= branch-remote "."))
                           (completing-read
                            "Remote Branch: "
                            (let ((rbs (magit-list-remote-branch-names)))
                              (mapcar
                               #'(lambda (rb)
                                   (and (string-match (rx bos
                                                          (one-or-more (not (any "/")))
                                                          "/"
                                                          (group (one-or-more any))
                                                          eos)
                                                      rb)
                                        (concat "refs/heads/" (match-string 1 rb))))
                               rbs)))
                         (and branch (magit-get "branch" branch "merge"))))
         (branch-pub (progn
                       (string-match (rx "refs/heads/" (group (one-or-more any)))
                                     branch-merge)
                       (format "refs/for/%s" (match-string 1 branch-merge)))))

    (when (or (null branch-remote)
              (string= branch-remote "."))
      (setq branch-remote magit-gerrit-remote))

    (magit-run-git-async "push" "-v" branch-remote
                         (concat "HEAD:" branch-pub))))
