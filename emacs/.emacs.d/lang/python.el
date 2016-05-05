
;;Python
(add-hook 'python-mode 'run-python) ; starts inferior python process
                                        ;(remove-hook 'python-mode-hook 'run-python)
(autoload 'jedi:setup "jedi" nil t)
;;(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(defun my/python-mode-hook()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
                                        ;(eval-after-load "company"
                                        ;'(progn
                                        ;(add-to-list 'company-backends 'company-jedi)))

(el-init-provide)
