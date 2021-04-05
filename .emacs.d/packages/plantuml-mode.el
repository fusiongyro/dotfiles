(use-package plantuml-mode
  :ensure t
  :mode "\\.puml\\'"
  :defines plantuml-jar-path
  :config
  (use-package flycheck-plantuml :ensure t :demand t)
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))
