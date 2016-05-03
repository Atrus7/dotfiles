(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(el-init-provide)
