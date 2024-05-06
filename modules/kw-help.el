;; -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :defer 0
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  (which-key-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

(provide 'kw-help)
