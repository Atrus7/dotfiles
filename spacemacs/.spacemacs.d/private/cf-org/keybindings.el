
(evil-define-key 'normal org-mode-map
  (kbd "C-k") 'org-insert-link)
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-1") (lambda () (interactive) (org-ctrl-c-tab 0)))
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-2") (lambda () (interactive) (org-ctrl-c-tab 1)))
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-3") (lambda () (interactive) (org-ctrl-c-tab 2)))
(evil-define-key '(insert normal) org-mode-map
  (kbd "M-4") (lambda () (interactive) (org-ctrl-c-tab 3)))


;; unbind the middle mouse key. When typing on laptop, I keep pressing it accidentally, triggering a yank.
(evil-define-key '(normal insert visual) org-mode-map  [mouse-2] 'ignore)

;; Somehow org-mode messes this up, making it Org-shift-down. Explicitly remap it
(evil-define-key '(normal) org-mode-map (kbd "J") 'evil-join)
