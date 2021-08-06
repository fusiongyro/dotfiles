(use-package org-roam
  :ensure t
  :demand t
  :defines org-roam-directory
  :init
  (setq org-roam-directory "~/Sync/Slipbox"
	org-roam-v2-ack t)
  :config
  (org-roam-setup))

(provide 'org-roam)
;;; org-roam ends here
