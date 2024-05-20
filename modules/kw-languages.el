;; -*- lexical-binding: t; -*-

;; Go
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun kw/go-install-save-hooks ()
  "Format and organize imports and buffer on save."
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  (add-hook 'before-save-hook #'eglot-code-actions t t))

(defun kw/godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (completing-read "Package: " (go-packages))))

;; Uncomment and fix the binding if you want to use it
(eval-after-load 'go-mode
  '(progn
     (define-key go-mode-map (kbd "C-c g d") 'kw/godoc-package)))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  ((go-mode . eglot-ensure)                     ; Enable eglot for Go files
   (before-save . kw/go-install-save-hooks)     ; Install save hooks
   (go-mode . flycheck-golangci-lint-setup))    ; Setup flycheck for linting
  :config
  ;; Set indentation settings
  (setq go-indent-offset 4                      ; Indentation offset
        gofmt-command "goimports"               ; Use goimports for formatting
        indent-tabs-mode nil)                   ; Use spaces instead of tabs

  ;; Set default tab width
  (setq-default tab-width 4)

  ;; Enable flycheck with golangci-lint
  (use-package flycheck-golangci-lint
    :ensure t)

  ;; Enable company mode for auto-completion
  (company-mode t))

(use-package company-go
  :after go-mode
  :ensure t
  :config (add-hook 'go-mode-hook 'company-mode))

(use-package go-eldoc
  :ensure t
  :requires go-mode
  :hook (go-mode . go-eldoc-setup))

(use-package gotest
  :requires go-mode
  :hook (go-mode . gotest))

(use-package golint
  :requires go-mode
  :hook (go-mode . golint))

;; Zig
(use-package zig-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig?\\'" . zig-mode))
  (which-key-add-major-mode-key-based-replacements 'zig-mode
    "C-c t" "Testing"
    "C-c r" "Running"
    "C-c c" "Compiling"))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
  (add-hook 'json-mode #'flycheck-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode))

(unless (package-installed-p 'lua-mode)
  (use-package lua-mode
    :demand t
    :mode (("\\.lua'" . lua-mode))))


(provide 'kw-languages)
