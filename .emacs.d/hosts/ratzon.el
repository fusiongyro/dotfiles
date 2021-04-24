;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/stow/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :defines mu4e-user-mail-address-list send-mail-function smtpmail-smtp-server
  mu4e-mu-binary mu4e-sent-folder mu4e-drafts-folder mu4e-trash-folder
  mu4e-refile-folder mu4e-get-mail-command mu4e-html2text-command mu4e-update-interval
  mu4e-compose-signature mu4e-headers-fields mu4e-bookmarks mu4e-maildir mu4e-contexts
  :config
  (setq
   mu4e-maildir  "~/Mail"
   user-mail-address "fusion@storytotell.org"
   mu4e-compose-signature (concat "Daniel K Lyons\n")
   mu4e-sent-folder "/Clanspum/INBOX.Sent"
   mu4e-drafts-folder "/Clanspum/INBOX.Drafts"
   mu4e-trash-folder "/Clanspum/INBOX.Trash"
   mu4e-refile-folder "/Clanspum/INBOX.Old Mail"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-service 587
   smtpmail-local-domain "storytotell.org"
   smtpmail-sendto-domain "storytotell.org"
   smtpmail-smtp-server "csv5.clanspum.net"
   smtpmail-smtp-user "fusion"
   user-full-name "Daniel Lyons"
   send-mail-function 'smtpmail-send-it
   mu4e-get-mail-command "/home/dlyons/.local/bin/offlineimap"
   mu4e-html2text-command 'mu4e-shr2text
   mu4e-update-interval 600
   mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
   mu4e-bookmarks '(("maildir:/Clanspum/INBOX" "Inbox" ?i)
                    ("maildir:/Clanspum/Sent" "Sent Messages" ?s)
                    ("flag:unread AND NOT flag:trashed AND NOT maildir:/Clanspum/INBOX.Junk" "Unread messages" ?u)
                    ("date:today..now" "Today's messages" ?t)
                    ("date:1d..today" "Yesterday's messages" ?y)
                    ("date:7d..now" "Last 7 days" ?w)
                    ("mime:image/*" "Messages with images" ?p))
  mu4e-mu-binary "/home/dlyons/stow/bin/mu")
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
;  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode)
  (load-library "org-mu4e")
  (load-library "mu4e-contrib"))

(custom-set-variables
 '(j-console-cmd "/usr/bin/ijconsole"))

(provide 'kavannah)
;;; kavannah.el ends here
