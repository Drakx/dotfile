;; -*- lexical-binding: t; -*-

;; When we're developing some application we frequently interact with APIs.
;; There are applications like postman, httpie, insomnia and so on to accomplish this task
;; but having an external application only to test a few endpoints or even a complex API is a little overkill.
;; Using emacs and a great package called https://github.com/pashky/restclient.el
;; we can have a very complete tool to handle API requests
(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

;; For example usage create a new file called test.http within that file add the following
;;
;; GET http://localhost:5000/?name=guest
;; Content-Type: application/json


;; To execute this request use ~C-c C-c~ if you'd like to copy the curl request use ~C-c C-u~

;; This will then result in the following
;; Hello, guest!
;; <!-- GET http://localhost:5000/?name=guest -->
;; <!-- HTTP/1.0 200 OK -->
;; <!-- Content-Type: text/html; charset=utf-8 -->
;; <!-- Content-Length: 13 -->
;; <!-- Server: Werkzeug/0.16.0 Python/3.6.9 -->
;; <!-- Date: Tue, 29 Oct 2019 05:34:44 GMT -->
;; <!-- Request duration: 0.023261s -->

(provide 'kw-rest)
