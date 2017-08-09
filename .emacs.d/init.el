;;; Commentary:
;;init -- This is my init file.
;; -*- mode: emacs-lisp -*-

;;; Code:
(setq gc-cons-threshold 800000000)
(setq package-enable-at-startup nil)
(package-initialize)

(setq load-path (cons "~/.emacs.d/dkl" load-path))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(setq text-quoting-style 'curve)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'exec-path "/home/fox/stow/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; propagate the path variable in case it's stupid
(setenv "PATH" (mapconcat 'identity exec-path ":"))

(add-hook 'text-mode-hook 'visual-line-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Use-package init
;;
(eval-when-compile
  (require 'use-package))

;; a function: confirm, but only if the server isn't running
(defun confirm-if-server-running (query)
  "Require a confirmation if the server is running.  Something something QUERY."
  (if (and (fboundp 'server-running-p) (server-running-p))
      (y-or-n-p query)
    t))

(defun add-lisp-hook (hook)
  "Apply HOOK to each Lisp mode, of which there are many."
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

(use-package impatient-mode
  :ensure t
  :demand t)

(use-package smex
  :ensure t
  :demand t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package avy
  :ensure t
  :demand t
  :config
  (avy-setup-default)
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind
  ("C-'" . avy-goto-char-2))

(use-package cider)

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package ido-mode
  :defines ido-everywhere
  :demand t
  :init
  (ido-mode 1)
  :config
  (setq ido-everywhere t))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :demand t
;;   :init (add-hook 'text-mode-hook 'fci-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (use-package company-go :ensure t :demand t)
  (use-package go-eldoc :ensure t :demand t :init (go-eldoc-setup))
  (setenv "PATH" (concat (getenv "PATH")
                         ":" (getenv "GOROOT") "/bin"
                         ":" (getenv "GOROOT") "/bin"))
  (push (concat (getenv "GOPATH") "/bin") exec-path)
  (push (concat (getenv "GOROOT") "/bin") exec-path)
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v; and go test -v; and go vet; and golint")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))))
  :bind (("M-."     . godef-jump)
         ("C-c C-c" . compile)))

(use-package telephone-line
  :demand t
  :ensure t
  :init
  (telephone-line-mode 1))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-lisp-hook #'enable-paredit-mode)
  (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp))

(use-package flatui-theme
  :ensure t
  :demand t
  :init
  (load-theme 'flatui t))

;; (use-package material-theme
;;   :ensure t
;;   :demand t
;;   :init (load-theme 'material-light))

(use-package proof-site
  :defines proof-three-window-enable
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :config
  (setq proof-three-window-enable t)
  :load-path
  "/home/fox/stow/packages/ProofGeneral/generic")

(use-package rnc-mode
  :defines rnc-jing-jar-file
  :mode "\\.rnc\\'"
  :init
  (setq rnc-jing-jar-file (expand-file-name "~/jing.jar")))

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq flycheck-c/c++-gcc-executable "/home/fox/stow/packages/gcc-4.8.5/bin/gcc")
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Prolog stuff
(use-package prolog-mode
  :defines prolog-program-name prolog-indent-width prolog-paren-indent-p prolog-system
  :mode "\\.pl\\'"
  :init
  (setq prolog-program-name "swipl"
	prolog-indent-width 4
	prolog-paren-indent-p t
	prolog-system 'swi))
;; (load-file "/Users/fusion/Desktop/prolog.el")

;; GHC/Haskell stuff
;; (use-package ghc-mod
;;   :init
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   :config
;;   (ghc-init)
;;   (haskell-indentation-mode))

(use-package markdown-mode
  :ensure t
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

(use-package haste
  :ensure t)

;; Mail
(defun file-string (file)
    "Read the contents of FILE and return as a string."
    (with-current-buffer (find-file-noselect file)
      (buffer-string)))

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
        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archives"
        mu4e-get-mail-command "/usr/local/bin/offlineimap"
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
  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode)
  (load-library "org-mu4e")
  (load-library "mu4e-contrib"))

;; Window manager
;(load-file "~/.emacs.d/dkl/exwm.el")

;; printer
(setq lpr-switches '("-Paoc324"))

(diminish 'abbrev-mode)
(diminish 'auto-fill-function)
(diminish 'mml-mode)

;; Org mode stuff
(use-package org
  :demand t
  :diminish orgstruct-mode
  :diminish orgtbl-mode
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
  (add-hook 'message-mode-hook 'flyspell-mode)
  (add-hook 'message-mode-hook 'typopunct-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)))

;; Haste
(use-package haste
  :ensure t
  :bind ("C-c h" . haste))

(use-package calc
  :defines math-additional-units
  :init
  (setq math-additional-units '((GB "1024 * MB" "Giga Byte")
				(MB "1024 * KB" "Mega Byte")
				(KB "1024 * B" "Kilo Byte")
				(B nil "Byte"))))

(use-package projectile
  :ensure t
  :config
  (use-package projectile-speedbar
    :ensure t))

(use-package perspective
  :ensure t
  :config
  (use-package persp-projectile
    :ensure t))

;; kill a window
(defun kill-buffer-and-frame ()
  "Deletes both the frame and the buffer which is active in the frame."
  (interactive)
  (kill-buffer)
  (delete-frame))

;; increase/decrease font size on a Mac
(bind-key "s-+" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)
(bind-key "<f6>" 'compile)

;; fix undo
;(bind-key "C-z" 'undo)

;; need to use UTF-8 by default because it's 2015
(setq default-process-coding-system '(utf-8 . utf-8))
(setq buffer-file-coding-system 'utf-8)

;; Kill buffers and frames
(bind-key "C-x 5 k" 'kill-buffer-and-frame)

;; Hey, let's unfill!
(defun unfill ()
  "Undo the effect of a fill."
  (interactive)
  (let ((previous-fill-column fill-column))
    (setq fill-column 10000000)
    (fill-paragraph)
    (setq fill-column previous-fill-column)))

(bind-key "M-Q" 'unfill)

;; Let's have a Stack Overflow-ify method on the buffer and region
(defun copy-buffer-for-stackoverflow (beg end)
  "Kill the active region from BEG to END, or the active buffer, prepending 4 spaces to the beginning."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list nil nil)))
  (let* ((buf (if (and beg end) (buffer-substring-no-properties beg end) (buffer-string)))
	 (enhanced (replace-regexp-in-string "^" "    " buf)))
    (kill-new enhanced)))

(bind-key "C-c s" 'copy-buffer-for-stackoverflow)

(bind-key "C-c t" 'auto-revert-mode)

;; alt keybindings from Mac OS X
(bind-key "M-_" "—")
;(bind-key "M-(" "‘")
;(bind-key "M-)" "’")
(bind-key "M-/" 'hippie-expand)

;; fixing problems on OS X
(bind-key "<home>" 'beginning-of-line)
(bind-key "<end>" 'end-of-line)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; this is for the shell, because it isn't brilliant at this
(setenv "HISTFILE" (expand-file-name (format "~/.history/%s" (getenv "HOSTNAME"))))

(setq gc-cons-threshold 800000)

(load-library "ayu-light-theme")

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
 '(column-number-mode 1)
 '(confirm-kill-emacs (quote confirm-if-server-running))
 '(custom-enabled-themes (quote (ayu-light)))
 '(custom-safe-themes
   (quote
    ("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "2e082aef340057efbbb5c9db06f5eebf177641ed25ac15e1c75af298ec25a107" default)))
 '(desktop-save-mode t)
 '(display-time-mode 1)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#f1c40f")
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode 1)
 '(line-spacing 4)
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (xah-fly-keys smex god-mode projectile-speedbar speedbar-projectile sr-speedbar persp-projectile treemacs perspective-projectile projectile-perspective perspective projectile org-plus-contrib lua-mode smooth-scroll elm-mode use-package telephone-line sml-mode slime-company paredit markdown-mode magit impatient-mode haste graphviz-dot-mode go-eldoc flycheck flatui-theme fill-column-indicator company-go company-ghc cider org alert haskell-mode)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(smtpmail-default-smtp-server "smtp-auth.aoc.nrao.edu")
 '(smtpmail-local-domain "nrao.edu")
 '(smtpmail-sendto-domain "nrao.edu")
 '(smtpmail-smtp-server "smtp-auth.aoc.nrao.edu")
 '(smtpmail-smtp-user "dlyons")
 '(starttls-extra-arguments nil)
 '(starttls-gnutls-program "/opt/local/bin/gnutls-cli")
 '(starttls-use-gnutls t)
 '(tab-width 4)
 '(tags-revert-without-query 1)
 '(tool-bar-mode nil)
 '(typopunct-buffer-language (quote english))
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9")
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "PragmataPro Mono"))))
 '(fringe ((t (:background "gray100"))))
 '(variable-pitch ((t (:height 150 :family "Source Sans Pro")))))

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
