;; -*- lexical-binding: t; -*-


;; This is so messed up it's crazy, depending on the keyboard layout chosen and weather it's macOS or Linux (don't care about Windows).
;; The keyboard shortcut changes for ~#~ and ~£~ for UK users. Emacs by default has ~M-3~ bound to something else.
;; Since I'm so use to using ~M-3~ to give me '#' (American keyboard layout, Apple calls it 'British') when using a mac,
;; I find it odd to press ~S-3~ and not get '£' that I've decided I should make Emacs do what I want when I want.

;; Note for this to work, your keyboard should be Apple MacBook Pro (intl) set to English UK
;; Allow hash to be entered when using a mac
(pcase system-type
  ('darwin
   (message "Running on macOS")
   (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#"))))
  ('gnu/linux
   (message "Running on Linux")
   (global-set-key (kbd "S-3") (lambda () (interactive) (insert "#")))
   (global-set-key (kbd "M-3") (lambda () (interactive) (insert "£")))))

(provide 'kw-keyboard)
