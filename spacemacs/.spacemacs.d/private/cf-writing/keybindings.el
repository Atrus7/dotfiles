(evil-leader/set-key (kbd "o v i") 'cf/vlc-grab-timestamp)
(evil-leader/set-key (kbd "o v v") 'cf/vlc-toggle-pause)
(evil-leader/set-key (kbd "o w d") 'cf/write-today)
(evil-leader/set-key (kbd "x t") 'mw-thesaurus-lookup-dwim)
(evil-leader/set-key (kbd "x d") 'define-word-at-point)
(evil-leader/set-key (kbd "o w n") 'narrow-todays-indirect-buffer)

(evil-leader/set-key (kbd "t w") 'spacemacs/toggle-cf/writing-mode)
;; this will be "editing mode" eventually
(evil-leader/set-key (kbd "t e") 'spacemacs/toggle-cf/editing-mode)
