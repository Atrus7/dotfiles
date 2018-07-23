(spacemacs/set-leader-keys (kbd "t i")
  'spacemacs/toggle-cf/ide-mode)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  (kbd "s d") 'xref-find-definitions)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  (kbd "s D") 'xref-find-definitions-other-window)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  (kbd "s r") 'xref-find-references)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  (kbd "s e") 'lsp-rename)
