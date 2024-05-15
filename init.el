;; -*- coding: utf-8 -*-
;;
;; A list of helpful key binds
;; | Key combination | Action                                  |
;; |-----------------+-----------------------------------------|
;; | C-c C-l         | Create Hyperlink in org-mode            |
;; | C-c C-o         | Open link at point                      |
;; | C-c C-v t       | Tangle org-mode elips code              |
;; | C-c C-c         | Align table in org-mode                 |
;; | C-x /           | Search some sites with input from emacs |
;; | C-j             | Jump down to next bullet point in org   |
;; | C-k             | Jump up to next bullet point in org     |
;; | C-c t           | Open vterm                              |
;; | C-c x           | Open multi-vterm                        |
;; | C-SPC           | Increase font size                      |
;; | C-SPC           | Choose a theme                          |
;; | M-SPC           | Mac OS Counsel app launcher             |
;; | M-/             | Comment code using evil commenter       |
;; | C-c C-c         | Execute a request with restclient       |
;; | C-c C-u         | Copy the curl request in restclient     |
;; | C-c l d         | Access Dap Hydra                        |
;; | C-x g           | Magit/Git status                        |
;; | C-.             | Embark actions                          |
;; | C-d             | Embark act                              |
;; | C-% (C-shift 5) | Jump to matching brackets and back      |
;;
;;
;; Add configuration modules to load path
(add-to-list 'load-path '"~/.emacs.d/modules")
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'kw-package)
(require 'kw-functions)
(require 'kw-emacs)
(require 'kw-completion)
(require 'kw-writing)
(require 'kw-help)
(require 'kw-devel)
(require 'kw-srccontrol)
(require 'kw-frames)
(require 'kw-fonts)
(require 'kw-shell)
(require 'kw-root-edit)

;; If you prefer evil mode, set kw/evil-enabled t
(setq kw/evil-enabled nil)
(when kw/evil-enabled
  (require 'kw-evil))

(when (eq system-type 'darwin)
  (require 'kw-mac))
(require 'kw-keyboard)
(require 'kw-rest)
(require 'kw-www)
(require 'kw-encryption)
(require 'kw-keyboard)
(require 'kw-org)
(require 'kw-org-roam)
