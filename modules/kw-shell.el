;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vterm
  :commands term
  :ensure t
  :config
  (defalias 'term 'vterm)
  (setq vterm-max-scrollback 100000)
  (global-set-key (kbd "C-c t") 'vterm))

;; Allow for multiple vterms
(use-package multi-vterm
  :ensure t
  :commands multi-vterm
  :config
  (global-set-key (kbd "C-c x") 'multi-vterm))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Testing...
(unless (package-installed-p 'eat)
  (use-package eat
    :demand t
    :config
    (setq eat-kill-buffer-on-exit t
	  eat-enable-mouse t)))

(provide 'kw-shell)
