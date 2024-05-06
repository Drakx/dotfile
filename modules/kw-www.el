;; -*- lexical-binding: t; -*-

;; Usage C-x / d
(use-package engine-mode
  :config (engine-mode t)
  (defengine duckduckgo "https://duckduckgo.com/?q=%s"
             :keybinding "d")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s"
             :keybinding "s")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s"
             :keybinding "g"))

;;To open a url in =org-mode= you can use ~org-open-at-point~ or the keybind ~C-c C-o~
(setq my-browsers '(("Firefox" . browse-url-firefox)
                    ("Chromium" . browse-url-chromium)
                    ("Chrome" . browse-url-chrome)
                    ("EWW" . eww-browse-url)))

(when (eq system-type 'darwin)
  (setq browse-url-chrome-program
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (setq browse-url-firefox-program
        "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"))

(defun kw/my-browse-url (&rest args)
  "Select the preferred browser from a menu before opening the URL."
  (interactive)
  (let ((browsers (if (eq system-type 'darwin)
                      my-browsers
                    (delete '("Chrome" . browse-url-chrome) my-browsers)))
        (browser (ivy-read "WWW browser: " browsers
                           :require-match t)))
    (setq browse-url-browser-function (cdr (assoc browser my-browsers)))
    (apply browse-url-browser-function args)))

(setq browse-url-browser-function #'kw/my-browse-url)

;; Gopher/Gemini
;; I've just kinda found out about this, it's a protocol for 'text' based browsing,
;; no ads, no JS, no tracking, etc, etc that you'll find on the normal internet.
;; Seems kinda cool, so I figured I'd give it look and see what happens. List of [[https://gemini.circumlunar.space/clients.html][Gemini Clients]]
;; https://git.carcosa.net/jmcbray/gemini.el just gives some syntax highlighting
(use-package elpher)
(use-package gemini-mode
  :mode "\\.gmi\\'")

;; IRC
;; The following code is taken from https://systemcrafters.net as its a really good starting foundation
(use-package erc
  :demand t
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "Kai"
        erc-user-full-name "Kai"
        erc-track-shorten-start 8
        erc-autojoin-channels-alist '(
                                      ("irc.libera.chat"
                                       "#systemcrafters"
                                       "#emacs"))
        erc-kill-buffer-on-part t
        erc-auto-query 'bury
        erc-fill-column 120
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(provide 'kw-www)
