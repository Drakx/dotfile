;; -*- lexical-binding: t -*-
;;
;; Main configuration of Emacs' UI along with some themes

(use-package emacs
  :ensure nil
  :custom
  (completion-ignore-case t)                               ;; For completions ingore case
  (confirm-nonexistent-file-or-buffer nil)                 ;; Don't ask us to create the new file, just do it
  :config
  (tool-bar-mode      0)                                   ;; Remove toolbar
  (scroll-bar-mode    0)                                   ;; Remove scrollbar
  (menu-bar-mode      0)                                   ;; Remove menu bar
  (blink-cursor-mode  0)                                   ;; Remove Solid cursor
  (tooltip-mode -1)                                        ;; Disable tool tips
  (set-fringe-mode 10)                                     ;; Give some breathing room
  (display-time-mode 1)                                    ;; Make sure the time is displayed
  (global-auto-revert-mode 1)                              ;; Allow for Emacs to detect a file has changed outside Emacs
  (add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing white space on save

  (setq locale-coding-system        'utf-8                 ;; Set encoding
        set-terminal-coding-system  'utf-8-unix            ;; Set encoding
        set-keyboard-coding-system  'utf-8-unix            ;; Set encoding
        set-selection-coding-system 'utf-8                 ;; Set encoding
        prefer-coding-system        'utf-8                 ;; Set encoding
	visible-bell                nil                    ;; Disable visual bell
	vc-follow-symlinks          t                      ;; Follow version control symlinks automatically
        inhibit-startup-message     t                      ;; Disable starup message
        vc-follow-symlinks          t                      ;; Follow symlinks automatically
	backup-by-copying           t
        version-control             t                      ;; Use version numbers on backups
        delete-old-versions         t                      ;; Automatically delete excess backups
        kept-new-versions           10                     ;; how many of the newest versions to keep
        kept-old-versions           5
	display-time-24hr-format    t                      ;; Give me 24 hour clock
        display-time-format "%H:%M %a, %d %b %Y"           ;; Formatted to UK standard
        backup-directory-alist `(("." . "~/.emacs.d/auto-saves"))
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
	make-backup-files           nil                    ;; Stop making ~
        )

  (defun kw/config-visit ()
    "Visit the Emacs init.el file"
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  (defun kw/reload-emacs-with-new-changes ()
    "Reload the Emacs init.el"
    (interactive)
    (load-file "~/.emacs.d/init.el"))

  :init
  (defalias 'yes-or-no-p 'y-or-n-p)                         ;; Remove the need to type 'yes' over 'y'
  (defmacro k-time (&rest body)
    "Measure and return the time it takes evaluating BODY."
    `(let ((time (current-time)))
       ,@body
       (float-time (time-since time)))))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)                              ;; Start doom mode line
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15)
           (setq doom-modeline-mu4e t)                      ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
           (mu4e-alert-enable-mode-line-display)            ;; also enable the start of mu4e-alert
           (setq doom-modeline-icon t)))

;; If we are on a laptop then display the battery in the mode line.
;; 'kw/laptop-p' located in 'kw-functions.el'
(if (kw/laptop-p)
    (message "Laptop")
    (progn
      ;; Display the battery in the modeline
      (use-package battery
	:ensure t
	:hook (after-init . display-battery-mode)))
    (message "Desktop"))

;; Global key binds
(global-set-key (kbd "C-c c e") 'kw/config-visit)
(global-set-key (kbd "C-c c r") 'kw/reload-emacs-with-new-changes)

;; Theme
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))

(use-package all-the-icons-completion
  :ensure t
  :after all-the-icons
  :config
  (all-the-icons-completion-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :defer 1
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(provide 'kw-emacs)
