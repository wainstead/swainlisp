(require 'org)

;; Move this; and put this stuff under version control please.
;; Pilfered from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files '("~/Documents/GTD/inbox.org"
                         "~/Documents/GTD/gtd.org"
                         "~/Documents/GTD/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/GTD/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Documents/GTD/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ("w" "Waiting [inbox]" entry
                               (file+headline "~/Documents/GTD/inbox.org" "Waiting")
                               "* WAITING %i%?")
			      ("Q" "Question" entry
			       (file+headline "~/Documents/GTD/inbox.org" "Question")
                               "* %i%? \n %U")
			      ))

;; For use in refiling tasks from inbox.org
(setq org-refile-targets '(("~/Documents/GTD/gtd.org" :maxlevel . 3)
                           ("~/Documents/GTD/someday.org" :level . 1)
                           ("~/Documents/GTD/tickler.org" :maxlevel . 2)
			   ("~/Documents/GTD/habits.org" :maxlevel . 2)
			   ))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
