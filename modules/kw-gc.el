;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 100000000)

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 10 1024 1024))))

(defvar k-gc-timer
  (run-with-idle-timer 600 t ;; 10 Mins in seconds
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))
