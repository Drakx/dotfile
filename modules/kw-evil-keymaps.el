;; -*- lexical-binding: t; -*-
;;
;; Leader is set in the kw-evil.el and is set to <SPC>
;;
;; I have no idea if this is going to work, so wont be included in init.el
;; I may need to use General.el


(general-create-definer my-leader-def
  :states '(normal visual instert emacs)
  :prefix "SPC"
  :non-normal-prefix "Shift-SPC")

(my-leader-def
 :states '(normal visual emacs)
 :keymaps 'override
 "g"  '(:ignore t :which-key "Git")
 "gs" '(magit-status :which-key "Show status")
 "gb" '(magit-branch :which-key "Show all branches")
 "gco" '(magit-checkout :which-key "Checkout branch")
 "gr" '(magit-rebase :which-key "Rebase on a branch")
 "gq" '(magit-rebase-squash :which-key "Squash commits")
 "gc" '(magit-commit :which-key "Commit")
 "gp" '(magit-push-popup :which-key "Push"))

(provide 'kw-evil-keymaps)
