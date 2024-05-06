;; -*- lexical-binding: t; -*-

(use-package epa-file
  :ensure nil
  :config
  (setq epa-file-encrypt-to '("kaiwindle@gmail.com"))
  :custom
  ;; 'silent to use symmetric encryption
  ;; nil to ask for users unless specified
  ;; t to always ask for a user
  (when (eq system-type 'darwin)
    (custom-set-variables '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")))
  (epa-file-select-keys nil))

(use-package org-crypt
  :ensure nil  ;; included with org-mode
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-crypt-key "kaiwindle@gmail.com"))


(provide 'kw-encryption)
