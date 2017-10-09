;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t) (fortran . t) ))

;; GTD

;; Org caputure
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/gtd/inbox.org")

(global-set-key (kbd "C-c C-g")
		(lambda() (interactive) (find-file "~/gtd/gtd.org")))
(global-set-key (kbd "C-c C-i")
		(lambda() (interactive) (find-file "~/gtd/inbox.org")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "SCHEDULED(u)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

;; Org-agenda (see https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; End
(provide 'setup-org-mode)
