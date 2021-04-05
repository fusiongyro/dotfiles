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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-plantuml yasnippet-snippets yasnippet yaml-mode visual-fill-column color-theme-sanityinc-tomorrow smex plantuml-mode ob-restclient org-plus-contrib markdown-mode magit gemini-mode flycheck fish-mode feature-mode fancy-battery diminish company use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
