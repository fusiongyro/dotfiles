;; -*- mode: emacs-lisp -*-
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Use-package init
;;
(eval-when-compile
  (require 'use-package))

(defun add-lisp-hook (hook)
  (dolist (mode '(lisp-mode-hook
		  emacs-lisp-mode-hook
		  eval-expression-minibuffer-setup-hook
		  ielm-mode-hook
		  lisp-mode-hook
		  lisp-interaction-mode-hook
		  scheme-mode-hook
		  slime-mode-hook
		  slime-repl-mode-hook))
    (add-hook mode hook)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package telephone-line
  :demand t
  :ensure t
  :init
  (telephone-line-mode 1))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-lisp-hook #'enable-paredit-mode))

(use-package flatui-theme
  :ensure t
  :demand t
  :init
  (load-theme 'flatui t))

(use-package ido-mode
  :demand t
  :init
  (ido-mode 1)
  (setq ido-everywhere t))

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Prolog stuff
(use-package prolog-mode
  :mode "\\.pl\\'"
  :init
  (setq prolog-program-name "swipl"))

;; GHC/Haskell stuff
(use-package ghc-mod
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  :config
  (ghc-init)
  (haskell-indentation-mode)
  (add-to-list 'exec-path "~/.cabal/bin"))

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package tex-mode
  :init
  (setq tex-default-mode 'plain-tex-mode))

(use-package slime
  :ensure t
  :load-path "slime"
  :commands slime
  :config
  (progn
    ;; Slime and Auto-Complete
    (use-package slime-company :ensure t)
    (slime-setup '(slime-fancy slime-company))
    (setq slime-net-coding-system 'utf-8-unix)
    (setq inferior-lisp-program "sbcl")))

(use-package cider
  :ensure t
  :init
  (add-to-list 'exec-path "~/bin"))

;; Mail
(defun file-string (file)
    "Read the contents of a file and return as a string."
    (with-current-buffer (find-file-noselect file)
      (buffer-string)))

(use-package mu4e
  :config
  (setq user-mail-address "dlyons@nrao.edu"
	mu4e-user-mail-address-list '("dlyons@nrao.edu" "dlyons@aoc.nrao.edu")
	send-mail-function 'sendmail-send-it
	mu4e-mu-binary "mu"
	mu4e-sent-folder "/Sent"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder "/Trash"
	mu4e-refile-folder "/Archives"
	mu4e-get-mail-command "offlineimap"
	mu4e-update-interval 300
	mu4e-compose-signature (file-string "~/.signature")
	mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
	mu4e-bookmarks '(("maildir:/INBOX" "Inbox" ?i)
			 ("maildir:/Sent" "Sent Messages" ?s)
			 ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
			 ("date:today..now" "Today's messages" ?t)
			 ("date:7d..now" "Last 7 days" ?w)
			 ("mime:image/*" "Messages with images" ?p)))
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode))

;; Window manager
;(load-file "~/.emacs.d/dkl/exwm.el")

;; printer
;(setq lpr-switches '("-Paoc324"))

;; Org mode stuff
(use-package org
  :demand t
  :diminish orgstruct-mode
  :init
  (setq org-agenda-files '("~/Dropbox/Notes/TODO.org")
	org-confirm-babel-evaluate nil
	org-confirm-babel-evaluate nil
	org-use-speed-commands t
	org-default-notes-file "~/Dropbox/Notes/TODO.org")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (sh . t)
     (haskell . t)
     (awk . t)
     (lisp . t)))
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-iswitchb)))

;; kill a window
(defun kill-buffer-and-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

;; fix undo
(bind-key "C-z" 'undo)

;; need to use UTF-8 by default because it's 2015
(setq default-process-coding-system '(utf-8 . utf-8))
(setq buffer-file-coding-system 'utf-8)

;; Kill buffers and frames
(bind-key "C-x 5 k" 'kill-buffer-and-frame)

;; Hey, let's unfill!
(defun unfill ()
  (interactive)
  (let ((previous-fill-column fill-column))
    (setq fill-column 10000000)
    (fill-paragraph)
    (setq fill-column previous-fill-column)))

(bind-key "M-Q" 'unfill)

;; alt keybindings from Mac OS X
(bind-key "M-_" "—")
(bind-key "M--" "–")
(bind-key "M-9" "“")
(bind-key "M-0" "”")
(bind-key "M-(" "‘")
(bind-key "M-)" "’")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode 1)
 '(display-time-mode 1)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode 1)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tags-table-list
   (quote
    ("/home/fox/stow/src/emacs-24.5/src" "/home/fox/stow/src/emacs-24.5/lisp")))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:height 180 :family "PT Sans")))))
