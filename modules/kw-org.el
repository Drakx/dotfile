;; -*- lexical-binding: t; -*-

;; Lets make headers (*) look better than (***)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; TODO: Ensure these are set within the org config
(defvar org-default-file-location "~/Dropbox/Org/Org/Personal/")
(defvar org-default-work-file-location "~/Dropbox/Org/Org/Work/")

(defun kw/org-mode-setup ()
  "Setup for `org-mode'."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun kw/capture-org-template ()
  "org-capture-template"
  (interactive)
  (org-capture nil))

(use-package org
  :commands (org-agenda org-capture)
  :hook (org-mode . kw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-src-fontify-natively t
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-startup-with-inline-images t
        org-log-into-drawer t
        org-agenda-files (append
                          (mapcar (lambda (file)
                                    (expand-file-name file org-default-file-location))
                                  '("Tasks.org" "Habits.org" "Birthdays.org"))
                          (list (expand-file-name "Exchange.org" org-default-work-file-location))))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-graph-column 60
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANC(k@)" "SKIP(s@)"))
        org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                                 ("NEXT" :foreground "blue" :weight bold)
                                 ("DONE" :foreground "forest green" :weight bold)
                                 ("WAITING" :foreground "orange" :weight bold)
                                 ("HOLD" :foreground "magenta" :weight bold)
                                 ("CANCELLED" :foreground "forest green" :weight bold)
                                 ("SKIP" :foreground "forest green" :weight bold))
        org-refile-targets (mapcar (lambda (file)
                                     (list (expand-file-name file org-default-file-location) :maxlevel 1))
                                   '("Archive.org" "Tasks.org")))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist '((:startgroup)
                        ;; Put mutually exclusive tags here
                        (:endgroup)
                        ("@errand" . ?E)
                        ("@home" . ?H)
                        ("@work" . ?W)
                        ("agenda" . ?a)
                        ("planning" . ?p)
                        ("publish" . ?P)
                        ("batch" . ?b)
                        ("note" . ?n)
                        ("idea" . ?i)))

  (setq org-capture-templates
        '(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Dropbox/Org/Org/Personal/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("j" "Journal Entries")
          ("jj" "Journal" entry (file+olp+datetree "~/Dropbox/Org/Org/Personal/Journal.org.gpg")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n" :clock-in :clock-resume :empty-lines 1)
          ("jm" "Meeting" entry (file+olp+datetree "~/Dropbox/Org/Org/Personal/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n" :clock-in :clock-resume :empty-lines 1)
          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Dropbox/Org/Org/Personal/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Dropbox/Org/Org/Personal/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
          ("f" "Finished book" table-line (file "~/Dropbox/Org/Org/Personal/Books.org")
           "| %^{Title} | %^{Author} | %u |")))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands '(("d" "Dashboard" ((agenda "" ((org-deadline-warning-days 7)
                                                                   (org-agenda-show-log t)))
                                                       (todo "NEXT" ((org-agenda-overriding-header
                                                                      "Next Tasks")))
                                                       (tags-todo "agenda/ACTIVE"
                                                                  ((org-agenda-overriding-header
                                                                    "Active Projects")))))
                                     ("n" "Next Tasks" ((todo "NEXT" ((org-agenda-overriding-header
                                                                       "Next Tasks")))))
                                     ("W" "Work Tasks" tags-todo "+work-email")

                                     ;; Low-effort next actions
                                     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                                      ((org-agenda-overriding-header "Low Effort Tasks")
                                       (org-agenda-max-todos 50)
                                       (org-agenda-files org-agenda-files)))
                                     ("w" "Workflow Status" ((todo "WAIT"
                                                                   ((org-agenda-overriding-header
                                                                     "Waiting on External")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "REVIEW"
                                                                   ((org-agenda-overriding-header
                                                                     "In Review")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "PLAN"
                                                                   ((org-agenda-overriding-header
                                                                     "In Planning")
                                                                    (org-agenda-todo-list-sublevels
                                                                     nil)
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "BACKLOG"
                                                                   ((org-agenda-overriding-header
                                                                     "Project Backlog")
                                                                    (org-agenda-todo-list-sublevels
                                                                     nil)
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "READY"
                                                                   ((org-agenda-overriding-header
                                                                     "Ready for Work")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "ACTIVE"
                                                                   ((org-agenda-overriding-header
                                                                     "Active Projects")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "COMPLETED"
                                                                   ((org-agenda-overriding-header
                                                                     "Completed Projects")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "CANCEL"
                                                                   ((org-agenda-overriding-header
                                                                     "Cancelled Projects")
                                                                    (org-agenda-files
                                                                     org-agenda-files)))
                                                             (todo "SKIP"
                                                                   ((org-agenda-overriding-header
                                                                     "Skipped Projects")
                                                                    (org-agenda-files
                                                                     org-agenda-files))))))))

  (define-key global-map (kbd "C-c c t") 'kw/capture-org-template)
  (global-set-key (kbd "C-c a") 'org-agenda)

;; This is needed as of Org 9.2
;; If this isn't installed try Alt-x list packages
;; and install 9.4 at the time of writing
;; Note if you want to use begin_example use left arrow symbol and e then tab to auto complete
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("gq" . "src graphql")))

(provide 'kw-org)
