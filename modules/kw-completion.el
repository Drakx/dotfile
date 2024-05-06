;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  (:map minibuffer-local-map
	("<Backspace>" . minibuffer-backward-kill))
  :custom
  (setq vertico-cycle t
        vertico-resize nil)
  :config
  (add-hook 'vertico-mode-hook
            (lambda ()
              (setq save-silently t)))
  :init
  (vertico-mode 1))

;; Vertico posframe isn't enabled by default. To enable change (kw/posframe-enabled nil) to (kw/posframe-enabled 1)
(setq kw/posframe-enabled nil)
(when kw/posframe-enabled
  (use-package vertico-posframe
    :config
    ;; Allows the minibuffer to be centred in the screen
    (vertico-posframe-mode 1)))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marignalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :hook
  (marginalia . all-the-icons-completion-mode)
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-s" . consult-line)
	 ("C-x b" . consult-buffer)
	 :map minibuffer-local-map
	 ("C-r" . consult-history)))

(use-package savehist
  :ensure t
  :config
  (setq history-length 25)
  :init
  (savehist-mode))

(use-package embark
  :after vertico
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("C-d" . embark-act)
         :map embark-region-map
         ("D" . denote-region))

  :config
  ;; Remove the mixed indicator to prevent the popup from being displayed
  ;; automatically
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

;; http://company-mode.github.io/ provides a nicer in-buffer completion interface than =completion-at-point=
;; which is more reminiscent of what you would expect from an IDE.
;; We add a simple configuration to make the keybindings a little more useful (=TAB= now completes the selection and initiates completion at the current location if needed).
;; We also use https://github.com/sebastiencs/company-box to further enhance the look of the completions with icons and better overall presentation.
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ;;("<tab>" . company-complete-selection)
              ("<tab>" . company-complete-common-or-cycle))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map)
  :hook ((emacs-lisp-mode
          python-mode
          go-mode
          zig-mode
          web-mode
          restclient-mode) . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))


(provide 'kw-completion)