(require 'org)

;; capture tasks easily
;;(setq org-default-notes-file "~/Dropbox/projects/GTD/notes.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/GTD/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("i" "Tickler" entry
                               (file+headline "~/Documents/GTD/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ("w" "Waiting [inbox]" entry
                               (file+headline "~/Documents/GTD/inbox.org" "Waiting")
                               "* WAITING %i%?")
			      ("Q" "Question" entry
			       (file+headline "~/Documents/GTD/inbox.org" "Question")
                               "* %i%? \n %U")
			      ))

;; org-mode uses this for highlighting a node (via org-mark-element);
;; but I use it for a prefix key
(define-key org-mode-map (kbd "M-a") nil)
;; org-mode uses this for highlighting the current heading
(define-key org-mode-map (kbd "M-h") 'ns-do-hide-emacs)
(define-key org-mode-map (kbd "A-<down>") 'refmin)
;; Move this; and put this stuff under version control please.
;; Pilfered from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-agenda-files '("~/Documents/GTD/inbox.org"
                         "~/Documents/GTD/gtd.org"
                         "~/Documents/GTD/tickler.org"
						 "~/Documents/GTD/minutes.org"
						 )
	  )

;; For use in refiling tasks from inbox.org
(setq org-refile-targets '(("~/Documents/GTD/gtd.org" :maxlevel . 3)
                           ("~/Documents/GTD/someday.org" :level . 1)
                           ("~/Documents/GTD/tickler.org" :maxlevel . 2)
						   ("~/Documents/GTD/minutes.org" :maxlevel . 2)
						   ("~/Documents/GTD/habits.org" :maxlevel . 2)
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
											"perl"
											"ruby"
											"sh"
											"sql"
											"tcl"
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
