(use-package visual-fill-column
  :ensure t
  :demand t
  :init
  (add-hook 'message-mode-hook 'turn-on-visual-fill-column-mode))
