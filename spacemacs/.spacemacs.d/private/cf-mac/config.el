(when (spacemacs/system-is-mac)
  (setq mac-command-key-is-meta t
        mac-pass-command-to-system nil ; avoid hiding with M-h
        )

  (add-to-list 'helm-boring-buffer-regexp-list "\\.DS_Store$")
  )
