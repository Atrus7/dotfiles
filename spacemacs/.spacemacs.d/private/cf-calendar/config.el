;; where diary is stored
(setq diary-file "~/org/diary-google")

;; Fancy diary display, by date
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
