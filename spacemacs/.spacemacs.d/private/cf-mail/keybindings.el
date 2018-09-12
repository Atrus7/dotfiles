
(defhydra hydra-summary-limit-menu (:color pink
                                           :hint nil)
  "
^Limit to^             ^Expand^           ^Quit
^^^^^^^^-----------------------------------------------------------------
_s_: subject           _/_: pop-limit        _q_: quit
_a_: author            _D_: include-dormant
_r_: recipient         _g_: insert-new
_A_: address
_m_: marks
_u_: unread
"
  ("s" gnus-summary-limit-to-subject :exit t)
  ("a" gnus-summary-limit-to-author :exit t)
  ("r" gnus-summary-limit-to-recipient :exit t)
  ("A" gnus-summary-limit-to-address :exit t)
  ("m" gnus-summary-limit-to-marks :exit t)
  ("/" gnus-summary-make-nnir-group :color pink :exit t)
  ("u" gnus-summary-limit-to-unread :exit t)
  ("D" gnus-summary-limit-include-dormant :exit t)
  ("g" gnus-summary-insert-new-articles :exit t)
  ("q" gnus-summary-pop-limit "pop limit" :exit t))


(defhydra hydra-message-goto (:hint nil)
  "
GOTO
^^^^^^^^-----------------------------------------------------------------
_t_: to        _c_: cc           _q_: quit
_b_: body      _s_: subject
"
  ("t" (insert-after-fn 'message-goto-to) :exit t)
  ("c" (insert-after-fn 'message-goto-cc) :exit t)
  ("b" (insert-after-fn 'message-goto-body) :exit t)
  ("s" (insert-after-fn 'message-goto-subject) :exit t)
  ("q" nil :color pink))

(defun cf/goto-next-header ()
  (interactive)
  (message-next-header)
  (message-beginning-of-header nil)
  (evil-insert 1)
  )

(evil-define-key '(normal insert)
  message-mode-map (kbd "S-TAB") 'hydra-message-goto/body)
(evil-define-key '(normal insert)
  message-mode-map (kbd "TAB") 'cf/goto-next-header
)

(evil-define-key '(insert)
  mu4e-compose-mode-map
  (kbd "C-l") 'message-tab)

(evil-leader/set-key (kbd "am") 'mu4e) ;; Default is aM

;; (evil-leader/set-key-for-mode 'message-mode "s" 'message-goto-subject
;;   (kbd "t") 'message-goto-to
;;   (kbd "c") 'message-goto-cc
;;   (kbd "b") 'message-goto-b)


(evilified-state-evilify gnus-summary-mode gnus-summary-mode-map
  (kbd "J") 'gnus-summary-next-article
  (kbd "K") 'gnus-summary-prev-article
  (kbd "<RET>") 'spacemacs/browse-nnrss-url
  (kbd "/") 'hydra-summary-limit-menu/body)
