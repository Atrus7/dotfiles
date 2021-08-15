
(evil-define-key 'normal org-mode-map
  (kbd "C-k") 'org-insert-link)

;; quick cuts to show certain views
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-1") (lambda () (interactive) (org-shifttab 1)))
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-2") (lambda () (interactive) (org-shifttab 2)))
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-3") (lambda () (interactive) (org-shifttab 3)))
;; show everything
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-4") (lambda () (interactive) (org-show-all '(headings drawers blocks))))

(evil-define-key '(normal) org-mode-map (kbd "<RET>") 'newline-below-point)

;; unbind the middle mouse key. When typing on laptop, I keep pressing it accidentally, triggering a yank.
(evil-define-key '(normal insert visual) org-mode-map  [mouse-2] 'ignore)

;; Somehow org-mode messes this up, making it Org-shift-down. Explicitly remap it
(evil-define-key '(normal) org-mode-map (kbd "J") 'evil-join)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "c" nil)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "c i" 'cf-clock-in)
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "c o" 'cf-clock-out)
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "s N" 'cf/org-find-and-narrow-to-subtree)

;; capital-w
(spacemacs/declare-prefix (kbd "W") "Word Count...")
(evil-leader/set-key (kbd "W c") 'count-words)
(evil-leader/set-key (kbd "W g") 'cf/git-count-uncommitted-words)
(evil-leader/set-key (kbd "W e") 'cf/org-count-exported-words)
