;; -*- lexical-binding: t; -*-

(defvar org-roam-user-dir "~/Dropbox/Org/Org/Notes")
(use-package emacsql
  :defer nil)

(use-package emacsql-sqlite
  :after emacsql
  :defer nil)

(use-package org-roam
  :after org
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-roam-user-dir))
  (org-roam-db-location (expand-file-name ".org-roam.db" org-roam-user-dir))
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n r" . kw/org-capture-template)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . completion-at-point))
  :config
  (setq org-roam-author "Kai Windle"
        org-roam-email "kaiwindle@gmail.com")
  (when (eq system-type 'darwin)
    (setq org-roam-graph-viewer "/usr/local/bin/dot"))
  (setq org-roam-capture-templates
        '(("c" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %T\n#+AUTHOR: ${author}\n#+EMAIL: ${email}")
           :unnarrowed t)))
  ;; for org-roam-buffer-toggle
  (add-to-list 'display-buffer-alist '("\\*org-roam\\*" (display-buffer-in-direction)
                                       (direction . right)
                                       (window-width . 0.33)
                                       (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(defun kw/org-capture-template ()
  "Invoke Org-roam capture or default capture."
  (interactive)
  (if (eq major-mode 'org-mode)
      (call-interactively 'org-roam-capture)
    (call-interactively 'org-capture)))

(with-eval-after-load 'org-roam
  (require 'org-roam-protocol))


(provide 'kw-org-roam)
