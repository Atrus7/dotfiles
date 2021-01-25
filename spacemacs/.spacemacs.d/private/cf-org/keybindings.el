
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
