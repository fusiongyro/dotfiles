(use-package fancy-battery
  :ensure t
  :demand t
  :init (add-hook 'after-init-hook #'fancy-battery-mode))
