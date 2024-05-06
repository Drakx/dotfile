;; -*- lexical-binding: t; -*-

(when (eq system-type 'gnu/linux)
  (use-package sudo-edit
    :ensure t
    :commands (sudo-edit)
    :config
    ;; Set sudo-edit-explanations to nil if you want to disable the explanation buffer
    (setq sudo-edit-explanations nil)
    ))

(provide 'kw-root-edit)
