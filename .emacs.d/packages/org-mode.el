(use-package org
  :ensure t
  :demand t
  :diminish orgtbl-mode
  :functions org-babel-do-load-languages
  :config
  (use-package ob-restclient :ensure t :demand t)
  :init
  (setq org-agenda-files '("~/Sync/Notes/TODO.org" "~/Sync/Notes/VLASS.org")
	org-confirm-babel-evaluate nil
	org-use-speed-commands t
	org-default-notes-file "~/Sync/Notes/TODO.org"
    org-confirm-babel-evaluate nil
    org-hide-leading-stars t
    org-src-fontify-natively t
    org-babel-load-languages '((sql . t) (haskell . t) (awk . t) (lisp . t))
    org-confirm-babel-evaluate nil
    org-hide-leading-stars t
    org-src-fontify-natively t
    org-hide-emphasis-markers t
    org-adapt-indentation nil
    org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
    org-cycle-separator-lines 1)
  (add-hook 'message-mode-hook 'orgtbl-mode)
  (add-hook 'message-mode-hook 'flyspell-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)))
