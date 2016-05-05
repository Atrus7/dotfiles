(require 'vimrc-mode)
(require 'rainbow-delimiters); byte-compile rainbow delimiters for speed

(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(el-init-provide)
