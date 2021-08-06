;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :ensure t
  :demand t
  :mode "\\.hs\\'"
  :mode "\\.lhs\\'")


(use-package lsp-mode
  :ensure t
  :demand t
  :defines lsp-keymap-prefix
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (haskell-mode . lsp))
  :commands lsp)


;; optionally
(use-package lsp-ui
  :ensure t
  :demand t
  :commands lsp-ui-mode)


(use-package lsp-haskell
  :ensure t
  :demand t)


(provide 'lsp-mode)
;;; lsp-mode ends here
