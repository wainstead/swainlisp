(require 'org)

;; capture tasks easily
;;(setq org-default-notes-file "~/Dropbox/projects/GTD/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;; org-mode uses this for highlighting a node (via org-mark-element);
;; but I use it for a prefix key
(define-key org-mode-map (kbd "M-a") nil)

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
					    "diff"
					    "emacs-lisp"
					    "example"
					    "html"
					    "lisp"
					    "perl"
					    "ruby"
					    "sql"
					    "yaml"
					    ) nil t))
     ))

  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
	   (beginstr (if (string= mode "example") "#+BEGIN " "#+BEGIN_SRC "))
	   (endstr   (if (string= mode "example") "#+END " "#+END_SRC "))

	   (resulting-text
	    (format "%s%s\n%s%s%s\n"
		    beginstr mode
		    (buffer-substring-no-properties beg end)
		    endstr mode)))
      (kill-region beg end)
      (insert resulting-text))
  ))
