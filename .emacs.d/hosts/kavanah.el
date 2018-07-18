(use-package mu4e
  :defines mu4e-user-mail-address-list send-mail-function smtpmail-smtp-server
  mu4e-mu-binary mu4e-sent-folder mu4e-drafts-folder mu4e-trash-folder
  mu4e-refile-folder mu4e-get-mail-command mu4e-html2text-command mu4e-update-interval
  mu4e-compose-signature mu4e-headers-fields mu4e-bookmarks
  :config
  (setq
   mu4e-maildir  "~/Mail"
   mu4e-contexts `(,(make-mu4e-context
                     :name "Home"
                     :match-func (lambda (msg)
                                   (when msg (s-starts-with-p "/Clanspum" (mu4e-message-field msg :maildir))))
                     :vars '((user-mail-address . "fusion@storytotell.org")
                             (mu4e-compose-signature . (concat "Daniel K Lyons\n"))
                             (mu4e-sent-folder . "/Clanspum/INBOX.Sent")
                             (mu4e-drafts-folder . "/Clanspum/INBOX.Drafts")
                             (mu4e-trash-folder . "/Clanspum/INBOX.Trash")
                             (mu4e-refile-folder . "/Clanspum/INBOX.Old Mail")
                             (smtpmail-stream-type . starttls)
                             (smtpmail-smtp-service . 587)
                             (smtpmail-local-domain . "storytotell.org")
                             (smtpmail-sendto-domain . "storytotell.org")
                             (smtpmail-smtp-server . "csv5.clanspum.net")
                             (smtpmail-smtp-user . "fusion")))
                   ,(make-mu4e-context
                     :name "Work"
                     :match-func (lambda (msg)
                                   (when msg (s-starts-with-p "/NRAO" (mu4e-message-field msg :maildir))))
                     :vars '((user-mail-address . "dlyons@nrao.edu")
                             (mu4e-sent-folder . "/NRAO/Sent")
                             (mu4e-drafts-folder . "/NRAO/Drafts")
                             (mu4e-trash-folder . "/NRAO/Trash")
                             (mu4e-refile-folder . "/NRAO/Archive")
                             (smtpmail-smtp-server . "smtp-auth.aoc.nrao.edu")
                             (smtpmail-stream-type . starttls)
                             (smtpmail-smtp-service . 587)
                             (smtpmail-local-domain . "nrao.edu")
                             (smtpmail-sendto-domain . "nrao.edu")
                             (smtpmail-smtp-server . "smtp-auth.aoc.nrao.edu")
                             (smtpmail-smtp-user . "dlyons")
                             (mu4e-compose-signature . (file-string "~/.signature")))))
   user-full-name "Daniel Lyons"
   send-mail-function 'smtpmail-send-it
   mu4e-get-mail-command "/usr/bin/offlineimap"
   mu4e-html2text-command 'mu4e-shr2text
   mu4e-update-interval 600
   mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
   mu4e-bookmarks '(("maildir:/NRAO/INBOX OR maildir:/Clanspum/INBOX" "Inbox" ?i)
                    ("maildir:/Clanspum/INBOX" "Inbox" ?p)
                    ("maildir:/NRAO/Sent OR maildir:/Clanspum/Sent" "Sent Messages" ?s)
                    ("maildir:/NRAO/INBOX" "NRAO" ?n)
                    ("flag:unread AND NOT flag:trashed AND NOT maildir:/Clanspum/INBOX.Junk" "Unread messages" ?u)
                    ("date:today..now" "Today's messages" ?t)
                    ("date:1d..today" "Yesterday's messages" ?y)
                    ("date:7d..now" "Last 7 days" ?w)
                    ("mime:image/*" "Messages with images" ?p)))
  mu4e-get-mail-command "/usr/bin/offlineimap"
  mu4e-mu-binary "/usr/bin/mu"
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
;  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode)
  (load-library "org-mu4e")
  (load-library "mu4e-contrib"))
