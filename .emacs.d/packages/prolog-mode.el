(defun prolog-program-name ()
  "I use SWI-Prolog."
  "swipl")

;; Prolog stuff
(use-package prolog-mode
  :defines prolog-program-name prolog-indent-width prolog-paren-indent-p prolog-system
  :mode "\\.pl\\'"
  :init
  (setq prolog-program-name "swipl"
        prolog-indent-width 4
        prolog-paren-indent-p t
        prolog-system 'swi))
