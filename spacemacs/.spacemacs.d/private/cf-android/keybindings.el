

(defhydra hydra-chris-link-to-file (:hint nil)
  "
Link to:
^^^^^^^^-----------------------------------------------------------------
_c_: cs        _p_: partner           _q_: quit
_i_: it
"
  ("c" cf/get-cs-path :exit t)
  ("i" (cf/get-gerrit-path "https://-it.com/") :exit t)
  ("p" (cf/get-gerrit-path  "https://-partner.com/") :exit t)
  ("q" nil :color pink))
(spacemacs/declare-prefix (kbd "o f") "Files")
(evil-leader/set-key (kbd "o f y") 'hydra-chris-link-to-file/body)


(evil-leader/set-key "fa" 'cf/goto-build-file)

;; right now I use "o w" for writing.
;; (spacemacs/declare-prefix (kbd "o w") "Web")
;; (evil-leader/set-key (kbd "o w e") 'cf/chrome--source-ident)

(evil-leader/set-key (kbd "o w w") 'cf/dispatch-chrome-url)

(spacemacs/declare-prefix (kbd "o c") "Compile")

(evil-leader/set-key (kbd "o c l") 'cf/helm-lunch)
(evil-leader/set-key (kbd "o c c") 'cf/compile)
