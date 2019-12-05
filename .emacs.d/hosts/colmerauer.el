;; nothing here

(add-to-list 'org-publish-project-alist
             '("workspace-architecture-org"
               :base-directory "~/Projects/wksp0/architecture"
               :base-extension "org"
               :publishing-directory "/ssh:builder.aoc.nrao.edu:/home/builder.aoc.nrao.edu/content/ssa/architecture/workspaces"
               :recursive t
               :publishing-function org-html-publish-to-html))
(add-to-list 'org-publish-project-alist
             '("workspace-architecture-static"
               :base-directory "~/Projects/wksp0/architecture"
               :base-extension "png\\|css"
               :publishing-directory "/ssh:builder.aoc.nrao.edu:/home/builder.aoc.nrao.edu/content/ssa/architecture/workspaces"
               :recursive t
               :publishing-function org-publish-attachment))
(add-to-list 'org-publish-project-alist
             '("workspace-architecture" :components ("workspace-architecture-org" "workspace-architecture-static")))

(use-package mu4e
  :defines mu4e-user-mail-address-list send-mail-function smtpmail-smtp-server
  mu4e-mu-binary mu4e-sent-folder mu4e-drafts-folder mu4e-trash-folder
  mu4e-refile-folder mu4e-get-mail-command mu4e-html2text-command mu4e-update-interval
  mu4e-compose-signature mu4e-headers-fields mu4e-bookmarks
  :config
  (setq user-mail-address "dlyons@nrao.edu"
        mu4e-user-mail-address-list '("dlyons@nrao.edu" "dlyons@aoc.nrao.edu")
        send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp-auth.aoc.nrao.edu"
        mu4e-mu-binary "/usr/local/bin/mu"
        mu4e-sent-folder "/Sent Messages"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archives"
        mu4e-get-mail-command "/usr/local/bin/offlineimap"
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-update-interval 300
        mu4e-compose-signature (file-string "~/.signature")
        mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
        mu4e-bookmarks '(("maildir:/INBOX" "Inbox" ?i)
                         ("maildir:/Sent or maildir:\"/Sent Messages\"" "Sent Messages" ?s)
                         ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:1d..today" "Yesterday's messages" ?y)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("maildir:/Drafts" "Drafts" ?d)
                         ("mime:image/*" "Messages with images" ?p)))
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
                                        ;  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode)
  (require 'smtpmail)
  (load-library "org-mu4e")
  (load-library "mu4e-contrib"))
