;;; fox -- stuff for my work account
;;; Commentary:


;;; Code:



(defun open-qsub-error ()
  "Opens the error file in a qsub email in the appropriate place."
  (interactive)
  (save-excursion
    (search-forward "Error_Path: ")
    (let* ((partial-path (buffer-substring (point) (line-end-position)))
           (path (concat "/ssh:vlapipe@" partial-path)))
      (find-file-other-window path))))

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
        mu4e-mu-binary "/home/fox/stow/bin/mu"
        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archives"
        mu4e-get-mail-command "/home/fox/stow/bin/offlineimap"
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-update-interval 300
        mu4e-compose-signature (file-string "~/.signature")
        mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
        mu4e-bookmarks '(("maildir:/INBOX" "Inbox" ?i)
                         ("maildir:/Sent" "Sent Messages" ?s)
                         ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:1d..today" "Yesterday's messages" ?y)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("maildir:/Drafts" "Drafts" ?d)
                         ("mime:image/*" "Messages with images" ?p)))
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
;  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode)
  (load-library "org-mu4e")
  (load-library "mu4e-contrib"))

;; printer
(setq lpr-switches '("-Paoc324"))

(custom-set-variables
 '(focus-follows-mouse t)
 '(mouse-autoselect-window t)
 '(j-console-cmd "/Applications/j805/bin/jconsole")
 '(powerline-image-apple-rgb nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :family "PragmataPro Mono"))))
 '(variable-pitch ((t (:height 110 :family "Source Sans Pro")))))

(provide 'fox)
;;; fox.el ends here
