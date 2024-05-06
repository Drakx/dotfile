;; -*- lexical-binding: t; -*-

(use-package ace-window
  :defer 0
  :ensure t
  :init (progn (global-set-key [remap other-window] 'ace-window)
               (custom-set-faces '(aw-leading-char-face ((t
                                                          (:inherit ace-jump-face-foreground
                                                                    :height 3.0)))))))

;; Splits
(use-package emacs
  :ensure nil
  :preface
  (defun kw/split-and-follow-horizontally()
    "Splits the window horizontally and then makes the cursor move to the new window"
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
  (defun kw/split-and-follow-vertically()
    "Splits the window vertically and then makes the cursor move to new window."
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") 'kw/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'kw/split-and-follow-vertically))

(provide 'kw-frames)
