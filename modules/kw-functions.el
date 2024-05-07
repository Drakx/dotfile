;; -*- lexical-binding: t -*-

(defun minibuffer-backward-kill (arg)
  "When the minibuffer is completing a file name delete up to the parent
folder, otherwise delete a character backwards"
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (delete-backward-char arg)))

(defun kw/magit-kill-all-buffers ()
  "Kill all Magit-related buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (string-prefix-p "magit" (buffer-name buffer))
            (kill-buffer buffer)))
        (buffer-list))
  (message "Magit-related buffers killed."))

;; Disable line numbers for some modes
(defun kw/disable-line-numbers-mode ()
  "Disable line numbers mode for various major modes."
  (dolist (mode '(org-agenda-finalize-hook
                  org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  erc-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook
                  vshell-mode-hook
                  deft-mode-hook
                  dired-mode-hook
                  elpher-mode-hook
                  mu4e-headers-mode-hook
                  mu4e-view-mode-hook
                  mu4e-main-mode-hook))
    (add-hook mode
              (lambda ()
                (display-line-numbers-mode 0)))))

;; Call the function to ensure the hook is set up
(kw/disable-line-numbers-mode)

;; Helper function to update packages
(defun my/package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (async-start
   ;; Asynchronous function
   `(lambda ()
      (package-refresh-contents)
      (let (upgrades)
        (cl-flet ((get-version (name where)
                    (let ((pkg (cadr (assq name where))))
                      (when pkg
                        (package-desc-version pkg)))))
          (dolist (package (mapcar #'car package-alist))
            (let ((in-archive (get-version package package-archive-contents)))
              (when (and in-archive
                         (version-list-< (get-version package package-alist)
                                         in-archive))
                (push (cadr (assq package package-archive-contents))
                      upgrades)))))
        upgrades))
   ;; Callback function
   `(lambda (result)
      (let ((upgrades result))
        (if upgrades
            (when (yes-or-no-p
                   (message "Upgrade %d package%s (%s)? "
                            (length upgrades)
                            (if (= (length upgrades) 1) "" "s")
                            (mapconcat #'package-desc-full-name upgrades ", ")))
              (async-start
               ;; Asynchronous function for package upgrade
               `(lambda ()
                  (dolist (package-desc upgrades)
                    (let ((old-package (cadr (assq (package-desc-name package-desc)
                                                   package-alist))))
                      (package-install package-desc)
                      (package-delete  old-package))))
               ;; Callback function for package upgrade
               (lambda (_)
                 (message "Package upgrade completed.")))))
        (message "All packages are up to date")))))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;; http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(defun kill-scratch-buffer ()
  "Kill and recreate the *scratch* buffer.
  If the *scratch* buffer is killed, this function recreates it with
  `lisp-interaction-mode` and hooks it into `kill-buffer-query-functions`
  to handle future kill attempts."
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; If the *scratch* buffer is killed, recreate it automatically
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kw/shrug ()
  "Insert an ASCII shurg at cursor."
  (interactive)
  (insert (format "¯\\_(ツ)_/¯")))

(defun kw/disapproval ()
  "Insert an ASCII disapproval face at cursor."
  (interactive)
  (insert (format "ಠ_ಠ")))

(defun kw/stop-debugging-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode 0)
  (dap-ui-mode 0)
  (dap-ui-controls-mode 0)
  (delete-other-windows) ;; hide all the dap UI. I might want to delete the buffers as well.
  )

(defun kw/debugging-mode ()
  (interactive)
  ;;(start-remote-delve) ;; I use Go, so need delve remote debugging server to be ready.
                         ;; I couldn't figure out how to replicate `dlv debug my/go/program.go` in the config templates.
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode)
  (dap-ui-controls-mode 1)
  (dap-ui-sessions)
  (dap-ui-locals)
  (dap-ui-breakpoints)
  (dap-ui-repl))

;; Should ask for email address and then use that input
;; for the encryption packages below
(defun kw/ask-for-email(email)
  "Ask for users email for encryption"
  (interactive "sEnter email address to use for encryption: "))

(defun kw/laptop-p ()
  "Check if the system is a laptop."
  (interactive)
  (string-match-p "battery" (shell-command-to-string "ls /sys/class/power_supply")))

(defun my-jump-to-matching-bracket ()
  "Jump to the matching bracket."
  (interactive)
  (cond
   ;; If the character under point is an opening bracket, jump to the matching closing bracket
   ((looking-at "\\s(\\|\\s{\\|\\s<") (forward-sexp 1))
   ;; If the character under point is a closing bracket, jump to the matching opening bracket
   ((looking-back "\\s)\\|\\s}\\|\\s>") (backward-sexp 1))
   ;; Otherwise, do nothing
   (t (message "Not on a bracket"))))

(global-set-key (kbd "C-%") 'my-jump-to-matching-bracket)

;; Disable line numbers for some modes
(defun kw/disable-line-numbers-mode ()
  "Disable line numbers mode for various major modes."
  (dolist (mode '(org-agenda-finalize-hook
                  org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  erc-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook
                  vshell-mode-hook
                  deft-mode-hook
                  dired-mode-hook
                  elpher-mode-hook
                  mu4e-headers-mode-hook
                  mu4e-view-mode-hook
                  mu4e-main-mode-hook))
    (add-hook mode
              (lambda ()
                (display-line-numbers-mode 0)))))

;; Call the function to ensure the hook is set up
(kw/disable-line-numbers-mode)

(provide 'kw-functions)
