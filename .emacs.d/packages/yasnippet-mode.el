(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets :ensure t :demand t))
