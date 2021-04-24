(use-package autorevert :diminish auto-revert-mode)
(use-package abbrev :diminish abbrev-mode)
(use-package mml :diminish mml-mode)

;; Let's have a Stack Overflow-ify method on the buffer and region
(defun copy-buffer-for-stackoverflow (beg end)
  "Kill the active region from BEG to END, or the active buffer, prepending 4 spaces to the beginning."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list nil nil)))
  (let* ((buf (if (and beg end) (buffer-substring-no-properties beg end) (buffer-string)))
	 (enhanced (replace-regexp-in-string "^" "    " buf)))
    (kill-new enhanced)))

(use-package emacs
  :init
  (bind-key "C-c s" 'copy-buffer-for-stackoverflow)
  (bind-key "C-c t" 'auto-revert-mode)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (setq gc-cons-threshold 800000)
  (setq load-path (cons "~/.emacs.d/dkl" load-path))

  (add-hook 'text-mode-hook 'visual-line-mode)

  (setq text-quoting-style 'curve)

  (add-to-list 'exec-path "/home/fox/stow/bin")
  (add-to-list 'exec-path "~/bin")
  (add-to-list 'exec-path "/usr/local/bin")

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; this is for the shell, because it isn't brilliant at this
  (setenv "HISTFILE" (expand-file-name (format "~/.history/%s" (getenv "HOSTNAME"))))

;; formerly customize options
  (setq buffer-file-coding-system 'utf-8
	column-number-mode 1
	custom-file (concat user-emacs-directory "/custom.el")
	default-process-coding-system '(utf-8 . utf-8)
	desktop-save-mode t
	display-time-mode 1
	indent-tabs-mode nil
	inhibit-startup-buffer-menu t
	inhibit-startup-screen t
	initial-scratch-message nil
	line-spacing 4
	make-backup-files nil
	menu-bar-mode nil
	scroll-bar-mode nil
	sentence-end-double-space nil
	tab-width 4
	vc-follow-symlinks t)

  (tool-bar-mode -1)

  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  (put 'dired-find-alternate-file 'disabled nil)

  (set-face-attribute 'default nil :family "PragmataPro Liga" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "PragmataPro Liga")
  (set-face-attribute 'variable-pitch nil :family "Optima" :height 140))
