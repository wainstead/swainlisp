;; Work laptop (REM-MAC-19585) specific configuration

(load-file "~/Documents/workfiles/lisp.el")

(setq org-agenda-files
      (append org-agenda-files
              '("~/Documents/GTD/jira-tickets.org"
                "~/Documents/GTD/kanban-project.org"
                "~/Documents/GTD/team-building.org")))

(setq org-capture-templates
      (append org-capture-templates
              '(("m" "Morning checklist"
                 entry (file+olp+datetree "~/Documents/GTD/morning_checklist.org")
                 (file "~/Documents/GTD/checklist-template.org"))
                ("f" "Additional morning checklist items for Fridays"
                 entry (file+olp+datetree "~/Documents/GTD/morning_checklist.org")
                 (file "~/Documents/GTD/checklist-template-fridays.org")))))
