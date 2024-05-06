;; -*- lexical-binding: t; -*-

;; Allow Emacs to go full screen in macOS
(use-package emacs
  :ensure nil
  :preface
  (when (eq system-type 'darwin)
    (defun mac-toggle-max-window ()
      (interactive)
      (set-frame-parameter nil
			   'fullscreen
			   (if (frame-parameter nil 'fullscreen)
			       nil
			     'fullboth)))))

;; Allow Emacs to open macOS apps via counsel
(use-package counsel-osx-app
  :bind* ("M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              "/Applications/Xcode.app/Contents/Applications")))


(provide 'kw-mac)
