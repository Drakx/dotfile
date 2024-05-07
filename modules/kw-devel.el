;; -*- lexical-binding: t; -*-

;; Paren Matching
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-use-smartparens-bindings))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         js2-mode))

;; Projects
(defvar project-location "~/Documents/D/") ; Location where I store projects

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'ivy)
   (projectile-globally-ignored-files '("TAGS" "\#*\#" "*~" "*.la"
                                        "*.o" "*.pyc" "*.elc" "*.exe"
                                        "*.zip" "*.tar.*" "*.rar" "*.7z"))
   (projectile-project-search-path '(project-location))
   (projectile-switch-project-action #'projectile-dired)))

(use-package flycheck
  :ensure t
  :hook ((python-mode
          go-mode
          web-mode
          zig-mode
          markdown-mode
          json-mode
          yaml-mode
          css-mode) . flycheck-mode))

(use-package yasnippet
  :ensure t
  :diminish
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

;; Emacs' built in commenting functionality =comment-dwim= (usually bound to =M=) doesn't always
;; comment things in the way you might expect so we use https://github.com/redguardtoo/evil-nerd-commenter][evil-nerd-commenter
;; to provide a more familiar behavior.
;; I've bound it to =M-/= since other editors sometimes use this binding but you could also replace Emacs' =M- ;= binding with this command.
(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; Languages
(require 'kw-languages)

;; Markdown
(use-package markdown-mode
  :after lsp
  :ensure t
  :hook (markdown-mode . visual-line-mode))


;; Yaml
(use-package yaml-mode
  :after lsp
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;; JSON
(use-package json-mode
  :after lsp
  :ensure t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
  (add-hook 'json-mode #'flycheck-mode))

;; Web stuff
(use-package web-mode
  :ensure t
  :after lsp
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-words-in-buffer ac-source-abbrev)))
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;; CSS
(use-package css-mode
  :after (web lsp)
  :requires (rainbow-mode)
  :diminish
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode)))

;; Debugging
;; Dap mode allows us to debug apps within emacs, for easier use run ~dap-hydra~ and you'll be
;; presented with a very easy to follow/use menu mode
;; The python stuff is taken from https://emacs-lsp.github.io/dap-mode/page/python-poetry-pyenv/
;; (use-package dap-mode
;;   :after lsp-mode
;;   :ensure t
;;   :commands dap-debug
;;   :hook ((lsp-mode . dap-mode)
;;          (python-mode . dap-ui-mode)
;;          (python-mode . dap-mode)
;;          (go-mode . dap-mode)
;;          (go-mode . dap-ui-mode))
;;   :config
;;   (require 'dap-hydra)
;;   (require 'dap-go)
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   (defun dap-python--pyenv-executable-find (command)
;;     (with-venv (executable-find "python")))
;;   (dap-go-setup)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1);; Enable mouse hover support
;;   (dap-ui-controls-mode 1)
;;   ;; Bind `C-c l d` to `dap-hydra` for easy access
;;   (general-define-key
;;    :keymaps 'lsp-mode-map
;;    :prefix lsp-keymap-prefix "d" '(dap-hydra t :wk "debugger"))
;;   ;;:custom dap-auto-configure-features '(sessions locals controls tooltip)
;;   ;;(dap-mode 1)

;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra))))

(provide 'kw-devel)
