(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode))
