;; -*- lexical-binding: t; -*-

;; You will most likely need to adjust this font size for your system!
(defvar kw/default-font-size
  (if (eq system-type 'gnu/linux)
      120
    140))

(defvar kw/default-variable-font-size kw/default-font-size)

(defun kw/install-fonts-on-mac ()
  "Install fonts on macOS using Homebrew if not found."
  (when (eq system-type 'darwin)
    (unless (file-exists-p "/usr/local/bin/brew")
      (message "Homebrew not found. Please install Homebrew first.")
      (error "Homebrew not found"))

    (let ((fonts '(("font-cask-installed" . "cask")
                   ("font-cantarell" . "cantarell")
                   ("font-fira-code" . "fira-code"))))
      (dolist (font fonts)
        (unless (shell-command-to-string (format "brew list --cask %s 2>/dev/null" (cdr font)))
          (message "Installing font: %s" (car font))
          (shell-command-to-string (format "brew install --cask %s" (cdr font))))))))

(defun kw/set-font-faces ()
  "Set font faces based on the operating system."
  (message "Setting faces!")
  (when (eq system-type 'darwin)
    (kw/install-fonts-on-mac)
    (add-to-list 'load-path "~/Library/Fonts"))

  (set-face-attribute 'default nil
		      :font "Fira Code"
		      :height kw/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
		      :font "Fira Code"
		      :height kw/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
		      :font "Cantarell"
		      :height kw/default-variable-font-size
		      :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame (kw/set-font-faces))))
  (kw/set-font-faces))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General
;; https://github.com/noctuid/general.el is used for easy keybinding configuration that integrates well with which-key.
;; This code was taken from the original Emacs from scratch and has only been updated once
;; If you've not watched the efs by David Wilson (System Crafters) on YouTube, go watch it, honestly its a really good series
;; That needs to be updated to use newer packages like Vertico and Elgot
(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (efs/leader-keys "t"
		   '(
		     :ignore t
		     :which-key "toggles")
		   "tt" '(counsel-load-theme :which-key "choose theme")))

;; Text Scaling
;; This is an example of using https://github.com/abo-abo/hydra to design a transient key binding for quickly adjusting the
;; scale of the text on screen.
;; We define a hydra that is bound to =C-s t s= and, once activated.
;; =j= and =k= increase and decrease the text scale.
;; You can press any other key (or =f= specifically) to exit the transient key map.
(use-package hydra
  :defer t)

(defhydra hydra-text-scale
	  (:timeout 4)
	  "scale text"
	  ("J" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished"
	   :exit t))

(general-define-key
 :keymaps '(normal insert visual emacs)
 :prefix "SPC"
 :global-prefix "C-SPC"
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(provide 'kw-fonts)
