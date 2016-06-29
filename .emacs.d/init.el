;; -*- mode: emacs-lisp -*-

;;; Code:
(setq gc-cons-threshold 800000000)
(setq package-enable-at-startup nil)
(package-initialize)

(setq load-path (cons "~/.emacs.d/dkl" load-path))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'exec-path "/home/fox/stow/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.cabal/bin")

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

(load-file "~/Downloads/ProofGeneral-4.2/ProofGeneral-4.2/generic/proof-site.el")

(use-package cider)

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

(use-package proof-site
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :config
  (setq proof-three-window-enable t)
  :load-path
  "/home/fox/stow/packages/ProofGeneral/generic")

(use-package rnc-mode
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
    "Read the contents of a file and return as a string."
    (with-current-buffer (find-file-noselect file)
      (buffer-string)))

(use-package mu4e
  :config
  (setq user-mail-address "dlyons@nrao.edu"
	mu4e-user-mail-address-list '("dlyons@nrao.edu" "dlyons@aoc.nrao.edu")
	send-mail-function 'sendmail-send-it
	mu4e-mu-binary "/home/fox/stow/bin/mu"
	mu4e-sent-folder "/Sent"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder "/Trash"
	mu4e-refile-folder "/Archives"
	mu4e-get-mail-command "/home/fox/stow/bin/offlineimap"
	mu4e-update-interval 300
	mu4e-compose-signature (file-string "~/.signature")
	mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22) (:thread-subject))
	mu4e-bookmarks '(("maildir:/INBOX" "Inbox" ?i)
			 ("maildir:/Sent" "Sent Messages" ?s)
			 ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
			 ("date:today..now" "Today's messages" ?t)
                         ("date:1d..today" "Yesterday's messages" ?y)
			 ("date:7d..now" "Last 7 days" ?w)
			 ("mime:image/*" "Messages with images" ?p)))
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (load-library "org-mu4e"))

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
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-iswitchb)))

;; Haste
(use-package haste
  :ensure t
  :bind ("C-c h" . haste))

(use-package calc
  :init
  (setq math-additional-units '((GB "1024 * MB" "Giga Byte")
				(MB "1024 * KB" "Mega Byte")
				(KB "1024 * B" "Kilo Byte")
				(B nil "Byte"))))

;; kill a window
(defun kill-buffer-and-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

;; increase/decrease font size on a Mac
(bind-key "s-+" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)

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

;; Let's have a Stack Overflow-ify method on the buffer and region
(defun copy-buffer-for-stackoverflow (beg end)
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
(bind-key "M--" "–")
(bind-key "M-9" "“")
(bind-key "M-0" "”")
(bind-key "M-(" "‘")
(bind-key "M-)" "’")

(defalias 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; this is for the shell, because it isn't brilliant at this
(setenv "HISTFILE" (expand-file-name (format "~/.history/%s" (getenv "HOSTNAME"))))

(setq gc-cons-threshold 800000)

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tags-revert-without-query 1)
 '(column-number-mode 1)
 '(display-time-mode 1)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(indent-tabs-mode nil)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(line-number-mode 1)
 '(menu-bar-mode nil)
 '(proof-assistant-home-page "http://coq.inria.fr/")
 '(proof-context-command "Print All. ")
 '(proof-find-and-forget-fn (quote coq-find-and-forget))
 '(proof-find-theorems-command "Search %s. ")
 '(proof-goal-command "Goal %s. ")
 '(proof-goal-command-p (quote coq-goal-command-p))
 '(proof-guess-command-line (quote coq-guess-command-line))
 '(proof-multiple-frames-enable nil)
 '(proof-nested-undo-regexp
   "\\(?:\\_<Axiom\\_>\\)\\|\\(?:\\_<Global\\s-+Variable\\_>\\)\\|\\(?:\\_<Global\\s-+Variables\\_>\\)\\|\\(?:\\_<Hint\\s-+Constructors\\_>\\)\\|\\(?:\\_<Hint\\s-+Extern\\_>\\)\\|\\(?:\\_<Hint\\s-+Immediate\\_>\\)\\|\\(?:\\_<Hint\\s-+Resolve\\_>\\)\\|\\(?:\\_<Hint\\s-+Rewrite\\_>\\)\\|\\(?:\\_<Hint\\s-+Unfold\\_>\\)\\|\\(?:\\_<Existing\\s-+Instance\\_>\\)\\|\\(?:\\_<Hypothesis\\_>\\)\\|\\(?:\\_<Hypotheses\\_>\\)\\|\\(?:\\_<Parameter\\_>\\)\\|\\(?:\\_<Parameters\\_>\\)\\|\\(?:\\_<Conjecture\\_>\\)\\|\\(?:\\_<Variable\\_>\\)\\|\\(?:\\_<Variables\\_>\\)\\|\\(?:\\_<Coercion\\_>\\)\\|\\(?:\\_<CoFixpoint\\_>\\)\\|\\(?:\\_<CoInductive\\_>\\)\\|\\(?:\\_<Class\\_>\\)\\|\\(?:\\_<Declare\\s-+Module\\_>\\)\\|\\(?:\\_<Definition\\_>\\)\\|\\(?:\\_<Program\\s-+Definition\\_>\\)\\|\\(?:\\_<Derive\\s-+Inversion\\_>\\)\\|\\(?:\\_<Derive\\s-+Dependent\\s-+Inversion\\_>\\)\\|\\(?:\\_<Example\\_>\\)\\|\\(?:\\_<Equations\\_>\\)\\|\\(?:\\_<Fixpoint\\_>\\)\\|\\(?:\\_<Program\\s-+Fixpoint\\_>\\)\\|\\(?:\\_<Function\\_>\\)\\|\\(?:\\_<Functional\\s-+Scheme\\_>\\)\\|\\(?:\\_<Inductive\\_>\\)\\|\\(?:\\_<Instance\\_>\\)\\|\\(?:\\_<Program\\s-+Instance\\_>\\)\\|\\(?:\\_<Let\\_>\\)\\|\\(?:\\_<Ltac\\_>\\)\\|\\(?:\\_<Record\\_>\\)\\|\\(?:\\_<Scheme\\_>\\)\\|\\(?:\\_<Structure\\_>\\)\\|\\(?:\\_<Add\\s-+Morphism\\_>\\)\\|\\(?:\\_<Add\\s-+Parametric\\s-+Morphism\\_>\\)\\|\\(?:\\_<Chapter\\_>\\)\\|\\(?:\\_<Corollary\\_>\\)\\|\\(?:\\_<Fact\\_>\\)\\|\\(?:\\_<Goal\\_>\\)\\|\\(?:\\_<Lemma\\_>\\)\\|\\(?:\\_<Program\\s-+Lemma\\_>\\)\\|\\(?:\\_<Module\\s-+Type\\_>\\)\\|\\(?:\\_<Module\\_>\\)\\|\\(?:\\_<Remark\\_>\\)\\|\\(?:\\_<Section\\_>\\)\\|\\(?:\\_<Theorem\\_>\\)\\|\\(?:\\_<Program\\s-+Theorem\\_>\\)\\|\\(?:\\_<Obligation\\_>\\)\\|\\(?:\\_<Next Obligation\\_>\\)\\|\\(?:\\_<Add\\s-+Parametric\\s-+Relation\\_>\\)\\|\\(?:\\_<BeginSubproof\\_>\\)\\|\\(?:\\_<EndSubproof\\_>\\)\\|\\(?:\\_<Add\\s-+Abstract\\s-+Ring\\_>\\)\\|\\(?:\\_<Add\\s-+Abstract\\s-+Semi\\s-+Ring\\_>\\)\\|\\(?:\\_<Add\\s-+Field\\_>\\)\\|\\(?:\\_<Add\\s-+Printing\\_>\\)\\|\\(?:\\_<Add\\s-+Printing\\s-+If\\_>\\)\\|\\(?:\\_<Add\\s-+Printing\\s-+Let\\_>\\)\\|\\(?:\\_<Add\\s-+Ring\\_>\\)\\|\\(?:\\_<Add\\s-+Semi\\s-+Ring\\_>\\)\\|\\(?:\\_<Add\\s-+Setoid\\_>\\)\\|\\(?:\\_<Arguments\\s-+Scope\\_>\\)\\|\\(?:\\_<Bind\\s-+Scope\\_>\\)\\|\\(?:\\_<Canonical\\s-+Structure\\_>\\)\\|\\(?:\\_<Local\\s-+Close\\s-+Scope\\_>\\)\\|\\(?:\\_<Close\\s-+Scope\\_>\\)\\|\\(?:\\_<Delimit\\s-+Scope\\_>\\)\\|\\(?:\\_<Export\\_>\\)\\|\\(?:\\_<Extraction\\s-+Inline\\_>\\)\\|\\(?:\\_<Extraction\\s-+NoInline\\_>\\)\\|\\(?:\\_<Extraction\\s-+Language\\_>\\)\\|\\(?:\\_<Generalizable\\s-+Variables\\_>\\)\\|\\(?:\\_<Generalizable\\s-+All\\s-+Variables\\_>\\)\\|\\(?:\\_<Identity\\s-+Coercion\\_>\\)\\|\\(?:\\_<Implicit\\s-+Arguments\\s-+Off\\_>\\)\\|\\(?:\\_<Implicit\\s-+Arguments\\s-+On\\_>\\)\\|\\(?:\\_<Implicit\\s-+Arguments\\_>\\)\\|\\(?:\\_<Implicit\\s-+Types\\_>\\)\\|\\(?:\\_<Import\\_>\\)\\|\\(?:\\_<Infix\\_>\\)\\|\\(?:\\_<Local\\s-+Notation\\_>\\)\\|\\(?:\\_<Notation\\_>\\)\\|\\(?:\\_<Obligation\\s-+Tactic\\_>\\)\\|\\(?:\\_<Local\\s-+Open\\s-+Scope\\_>\\)\\|\\(?:\\_<Open\\s-+Local\\s-+Scope\\_>\\)\\|\\(?:\\_<Open\\s-+Scope\\_>\\)\\|\\(?:\\_<Remove\\s-+Printing\\s-+If\\_>\\)\\|\\(?:\\_<Remove\\s-+Printing\\s-+Let\\_>\\)\\|\\(?:\\_<Require\\s-+Export\\_>\\)\\|\\(?:\\_<Require\\s-+Import\\_>\\)\\|\\(?:\\_<Require\\_>\\)\\|\\(?:\\_<Reset\\s-+Extraction\\s-+Inline\\_>\\)\\|\\(?:\\_<Save\\_>\\)\\|\\(?:\\_<Set\\s-+Extraction\\s-+AutoInline\\_>\\)\\|\\(?:\\_<Set\\s-+Extraction\\s-+Optimize\\_>\\)\\|\\(?:\\_<Set\\s-+Implicit\\s-+Arguments\\_>\\)\\|\\(?:\\_<Set\\s-+Strict\\s-+Implicit\\_>\\)\\|\\(?:\\_<Set\\s-+Printing\\s-+Synth\\_>\\)\\|\\(?:\\_<Set\\s-+Printing\\s-+Wildcard\\_>\\)\\|\\(?:\\_<Set\\s-+Printing\\s-+All\\_>\\)\\|\\(?:\\_<Set\\s-+Printing\\s-+Coercions\\_>\\)\\|\\(?:\\_<Set\\s-+Printing\\s-+Notations\\_>\\)\\|\\(?:\\_<Solve\\s-+Obligations\\_>\\)\\|\\(?:\\_<Tactic\\s-+Notation\\_>\\)\\|\\(?:\\_<Unset\\s-+Extraction\\s-+AutoInline\\_>\\)\\|\\(?:\\_<Unset\\s-+Extraction\\s-+Optimize\\_>\\)\\|\\(?:\\_<Unset\\s-+Implicit\\s-+Arguments\\_>\\)\\|\\(?:\\_<Unset\\s-+Strict\\s-+Implicit\\_>\\)\\|\\(?:\\_<Unset\\s-+Printing\\s-+Synth\\_>\\)\\|\\(?:\\_<Unset\\s-+Printing\\s-+Wildcard\\_>\\)\\|\\(?:\\_<Unset\\s-+Printing\\s-+Coercion\\_>\\)\\|\\(?:\\_<Axiom\\_>\\)\\|\\(?:\\_<Global\\s-+Variable\\_>\\)\\|\\(?:\\_<Global\\s-+Variables\\_>\\)\\|\\(?:\\_<Hint\\s-+Constructors\\_>\\)\\|\\(?:\\_<Hint\\s-+Extern\\_>\\)\\|\\(?:\\_<Hint\\s-+Immediate\\_>\\)\\|\\(?:\\_<Hint\\s-+Resolve\\_>\\)\\|\\(?:\\_<Hint\\s-+Rewrite\\_>\\)\\|\\(?:\\_<Hint\\s-+Unfold\\_>\\)\\|\\(?:\\_<Existing\\s-+Instance\\_>\\)\\|\\(?:\\_<Hypothesis\\_>\\)\\|\\(?:\\_<Hypotheses\\_>\\)\\|\\(?:\\_<Parameter\\_>\\)\\|\\(?:\\_<Parameters\\_>\\)\\|\\(?:\\_<Conjecture\\_>\\)\\|\\(?:\\_<Variable\\_>\\)\\|\\(?:\\_<Variables\\_>\\)\\|\\(?:\\_<Coercion\\_>\\)\\|\\(?:\\_<CoFixpoint\\_>\\)\\|\\(?:\\_<CoInductive\\_>\\)\\|\\(?:\\_<Class\\_>\\)\\|\\(?:\\_<Declare\\s-+Module\\_>\\)\\|\\(?:\\_<Definition\\_>\\)\\|\\(?:\\_<Program\\s-+Definition\\_>\\)\\|\\(?:\\_<Derive\\s-+Inversion\\_>\\)\\|\\(?:\\_<Derive\\s-+Dependent\\s-+Inversion\\_>\\)\\|\\(?:\\_<Example\\_>\\)\\|\\(?:\\_<Equations\\_>\\)\\|\\(?:\\_<Fixpoint\\_>\\)\\|\\(?:\\_<Program\\s-+Fixpoint\\_>\\)\\|\\(?:\\_<Function\\_>\\)\\|\\(?:\\_<Functional\\s-+Scheme\\_>\\)\\|\\(?:\\_<Inductive\\_>\\)\\|\\(?:\\_<Instance\\_>\\)\\|\\(?:\\_<Program\\s-+Instance\\_>\\)\\|\\(?:\\_<Let\\_>\\)\\|\\(?:\\_<Ltac\\_>\\)\\|\\(?:\\_<Record\\_>\\)\\|\\(?:\\_<Scheme\\_>\\)\\|\\(?:\\_<Structure\\_>\\)\\|\\(?:\\_<Add\\s-+Morphism\\_>\\)\\|\\(?:\\_<Add\\s-+Parametric\\s-+Morphism\\_>\\)\\|\\(?:\\_<Chapter\\_>\\)\\|\\(?:\\_<Corollary\\_>\\)\\|\\(?:\\_<Fact\\_>\\)\\|\\(?:\\_<Goal\\_>\\)\\|\\(?:\\_<Lemma\\_>\\)\\|\\(?:\\_<Program\\s-+Lemma\\_>\\)\\|\\(?:\\_<Module\\s-+Type\\_>\\)\\|\\(?:\\_<Module\\_>\\)\\|\\(?:\\_<Remark\\_>\\)\\|\\(?:\\_<Section\\_>\\)\\|\\(?:\\_<Theorem\\_>\\)\\|\\(?:\\_<Program\\s-+Theorem\\_>\\)\\|\\(?:\\_<Obligation\\_>\\)\\|\\(?:\\_<Obligations\\_>\\)\\|\\(?:\\_<Next Obligation\\_>\\)")
 '(proof-output-tooltips nil)
 '(proof-prog-name "/opt/local/bin/coqtop")
 '(proof-prog-name-guess t)
 '(proof-query-file-save-when-activating-scripting nil)
 '(proof-really-save-command-p (quote coq-save-command-p))
 '(proof-save-command "Save %s. ")
 '(proof-script-comment-end "*)")
 '(proof-script-comment-end-regexp "\\*)")
 '(proof-script-comment-start "(*")
 '(proof-script-comment-start-regexp "(\\*")
 '(proof-script-parse-function (quote coq-script-parse-function))
 '(proof-script-span-context-menu-extensions (quote coq-create-span-menu))
 '(proof-shell-start-silent-cmd "Set Silent. ")
 '(proof-shell-stop-silent-cmd "Unset Silent. ")
 '(proof-showproof-command "Show. ")
 '(proof-state-preserving-p (quote coq-state-preserving-p))
 '(proof-terminal-string ".")
 '(proof-three-window-mode-policy (quote hybrid))
 '(proof-tree-configured t)
 '(proof-tree-find-begin-of-unfinished-proof (quote coq-find-begin-of-unfinished-proof))
 '(proof-tree-get-proof-info (quote coq-proof-tree-get-proof-info))
 '(mouse-autoselect-window t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tags-table-list
   (quote
    ("/home/fox/stow/src/emacs-24.5/src" "/home/fox/stow/src/emacs-24.5/lisp")))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ecf0f1" :foreground "#2c3e50" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "PragmataPro"))))
 '(variable-pitch ((t (:height 120 :family "Source Sans Pro")))))

(provide 'init)
;;; init.el ends here
