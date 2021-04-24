;;; Commentary:
;;init -- This is my init file.
;; -*- mode: emacs-lisp -*-

;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(dolist (packagedef (directory-files "~/.emacs.d/packages/" nil "\\.el$"))
  (load (concat "~/.emacs.d/packages/" packagedef)))


(load-file (concat "~/.emacs.d/hosts/" (system-name) ".el"))

(provide 'init)
;;; init.el ends here
