(require 'org)

;; Capture tasks easily

;; I'm using 't' for "task" set in org-capture-templates
;;(setq org-default-notes-file "~/Dropbox/projects/GTD/notes.org")

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates '(("t" "Todo [inbox]"       entry (file+headline "~/Documents/GTD/inbox.org" "Items")       "* %i%?")
                              ("i" "Tickler"            entry (file+headline "~/Documents/GTD/tickler.org" "Tickler")   "* %i%? \n %U")
                              ("w" "Waiting [inbox]"    entry (file+headline "~/Documents/GTD/inbox.org" "Waiting")     "* WAITING %i%?")
                              ("r" "Question [inbox]"   entry (file+headline "~/Documents/GTD/inbox.org" "Question")    "* %i%? \n %U")
							  ;; for this entry, see: https://www.reddit.com/r/orgmode/comments/c26qja/capture_template_based_in_a_file/
							  ;; which explains how to load your template from a file
							  ;; also note the use of 'file+olp+datetree' which is magical and could be used for my LOG.org
							  ("m" "Morning checklist"  entry (file+olp+datetree "~/Documents/GTD/morning_checklist.org") (file "~/Documents/GTD/checklist-template.org"))
							  ("f" "Additional morning checklist items for Fridays"  entry (file+olp+datetree "~/Documents/GTD/morning_checklist.org") (file "~/Documents/GTD/checklist-template-fridays.org"))
                              ))

;; org-mode uses this for highlighting a node (via org-mark-element);
;; but I use it for a prefix key
(define-key org-mode-map (kbd "M-a") nil)

;; org-mode uses this for highlighting the current heading
(define-key org-mode-map (kbd "M-h") 'ns-do-hide-emacs)

;; Pilfered from
;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;; Notably 'someday.org' is not here because we do not want its items
;; showing up in "agenda buffers"
(setq org-agenda-files '("~/Documents/GTD/inbox.org"
                         "~/Documents/GTD/gtd.org"
                         "~/Documents/GTD/tickler.org"
						 "~/Documents/GTD/reference.org"
                         ;;"~/Documents/GTD/minutes.org"
                         "~/.emacs.d/emacs.org"
						 "~/wainstead/study-projects/subjects-and-progress.org"
                         )
      )

;; For use in refiling tasks from inbox.org (or anywhere probably)
;; maxlevel will search <= N for headings to file under
(setq org-refile-targets '(("~/Documents/GTD/gtd.org" :maxlevel . 1)
                           ("~/Documents/GTD/someday.org" :level . 1)
						   ("~/Documents/GTD/backlog.org" :level . 1)
                           ("~/Documents/GTD/tickler.org" :maxlevel . 1)
                           ("~/Documents/GTD/minutes.org" :maxlevel . 1)
                           ("~/Documents/GTD/habits.org" :maxlevel . 1)
						   ("~/Documents/GTD/reference.org" :maxlevel . 1)
						   ("~/Documents/GTD/done.org" :maxlevel . 1)
						   ("~/.emacs.d/emacs.org" :maxlevel . 1)
						   ("~/wainstead/study-projects/subjects-and-progress.org" :regexp . "learning")
               ))

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(r)" "OBSOLETE(o)")))

(setq org-todo-keyword-faces
      '(("DEFERRED" . (:foreground "red" :weight bold)) ("STARTED" . "yellow")
        ("CANCELLED" . (:foreground "red" :weight bold))))



;; Put begin/end strings around a region in org-mode
;; see https://stackoverflow.com/questions/14201740/replace-region-with-result-of-calling-a-function-on-region
(defun sw-org-format-example (mode)
  ;; as cool as this is -- it lets you enter lambdas -- we'll stick
  ;; with plain strings for now. Later: allow a choice that prompts
  ;; the user for a lambda. Also need: let user enter arbitrary mode
  ;; name.
  ;;(interactive "XFunction to apply to region: ")
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Format as: " '(
                                            "conf"
                                            "css"
                                            "diff"
                                            "emacs-lisp"
                                            "example"
                                            "expect"
                                            "hexl"
                                            "html"
                                            "javascript"
                                            "json"
                                            "lisp"
											"markdown"
                                            "perl"
											"python"
                                            "ruby"
                                            "sh"
                                            "sql"
                                            "tcl"
											"terraform"
                                            "typescript"
                                            "yaml"
                                            "xml"
                                            ) nil t))
     ))

  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (beginstr (if (string= mode "example") "#+BEGIN_" "#+BEGIN_SRC "))
           (endstr   (if (string= mode "example") "#+END_" "#+END_SRC "))

           (resulting-text
            (format "%s%s\n%s%s%s\n"
                    beginstr mode
                    (buffer-substring-no-properties beg end)
                    endstr mode)))
      (kill-region beg end)
      (insert resulting-text))
    ))


;; (defun sw-org-complete-and-file ()
;;   "Mark a TODO item done, or canceled, or whatever, then archive it"
;;   (interactive)
;;   (org-todo)
;;   (org-archive-subtree)
;;   (message "Item status changed, and item archived")
;;   )
