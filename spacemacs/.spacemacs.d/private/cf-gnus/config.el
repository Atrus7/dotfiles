(setq
 gnus-save-newsrc-file nil ;Don't Read or Write newsrc files
 gnus-read-newsrc-file nil
 ; CONFLICT~/.gnus is by default the gnus-init-file
 ;gnus-startup-file "~/.gnus/.newsrc"
 ;gnus-init-file "~/.gnus/.gnus"


 ;; Perhaps have a system-message function that calls notify-send with the given message and also writes to normal message buffer
 ;; In this case you could message when Gnus starts stawrting and when it finishes starting up.
 ;; ‘gnus-before-startup-hook’
 ;; ‘gnus-startup-hook’

 user-full-name "Christopher Findeisen"
 user-mail-address "cfindeisen7@gmail.com"

 ;; gnus-auto-select-first
 gnus-no-groups-message "All done, C. No messages to be displayed."

 gnus-select-method
       '(nnmaildir "gmail"
                   (directory "~/mail/")
                   (directory-files nnheader-directory-files-safe)
                   (get-new-mail nil))


 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

 gnus-ignored-from-addresses "Christopher Findeisen"

;;  '(nnmaildir "GMail" 
;;              (directory "~/Documents/mail/")
;;              (directory-files nnheader-directory-files-safe) 
;;              (get-new-mail nil)))


)

(define-key gnus-group-mode-map (kbd "vo")
  '(lambda ()
     (interactive)
     (shell-command "offlineimap&" "*offlineimap*" nil)))
