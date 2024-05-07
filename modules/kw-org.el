;; -*- lexical-binding: t; -*-

;; Lets make headers (*) look better than (***)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(provide 'kw-org)
