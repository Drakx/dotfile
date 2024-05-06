;; -*- lexical-binding: t; -*-

(use-package flyspell
  :ensure t
  :hook ((text-mode . flyspell-mode)
         (text-mode . writegood-mode)
         (prog-mode . flyspell-prog-mode)
         (org-mode . flyspell-mode))
  :config
  (setq ispell-dictionary "en_GB")
  (when (eq system-type 'darwin)
    (setq ispell-program-name "/usr/local/bin/aspell")
    (eval-after-load "flyspell"
      '(progn
         (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
         (define-key flyspell-mouse-map [mouse-3] #'undefined))))
  (when (eq system-type 'linux-gnu)
    (setq ispell-program-name "/usr/bin/aspell"))

  (set-face-underline 'flyspell-incorrect '(:color "#dc322f" :style line))
  (set-face-underline 'flyspell-duplicate '(:color "#e5aa00" :style line)))

(use-package auto-correct
  :ensure t
  :after flyspell
  :hook (text-mode . auto-correct-mode))

(use-package writegood-mode
  :after flyspell
  :bind ("C-c g" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))

(provide 'kw-writing)
