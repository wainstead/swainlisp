(require 'org)

;; Capture tasks easily

;; I'm using 't' for "task" set in org-capture-templates
;;(setq org-default-notes-file "~/Dropbox/projects/GTD/notes.org")

(define-key global-map "\C-cc" 'org-capture)

;; Curiously, org-sort-entries has no keybinding, so we put it on
;; contol-c-e.
(define-key org-mode-map "\C-ce" 'org-sort-entries)

;; https://orgmode.org/manual/Template-elements.html

;; Entries in org-capture-templates are "templates" for
;; org-capture. They are not some native Lisp type or anything. Per
;; the page above, each entry in the list is:

;; key (below, our keys are t, i, w, r, m, f; they are the menu
;; selection).

;; description (helpful display text when you are choosing from the
;; menu

;; type -- the type of entry. Valid choices are entry, item,
;; checkitem, table-line, and plain. checkitem inserts a checkbox;
;; table-line adds a new line to the table at that location.

;; target -- where the item should be placed. Usually, it's a node in
;; an Org file.

;; template -- defaults to "an appropriate default template," per the
;; manual. Otherwise it's a string with escape codes (e.g. "* %i%?"),
;; a file, or a function that returns the template.

;; The template is tricky; the manual could use some more
;; explication. If you pass a string it 1) has to start with an
;; asterisk, it seems; and 2) it MUST contain at least one escape
;; sequence (e.g. %U, or %i%?, etc.)

;; The escape codes are described in
;; https://orgmode.org/manual/Template-expansion.html

;; properties -- the manual lists many; interesting ones include
;; setting how many blank lines to add above/below or to prepend
;; instead of append to the list. For elisp property lists see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html

(setq org-capture-templates '(("t" "Todo [inbox]"    entry (file+headline "~/Documents/GTD/inbox.org" "Items")    "* %i%?")
                              ("w" "Waiting [inbox]" entry (file+headline "~/Documents/GTD/inbox.org" "Waiting")  "* WAITING %i%?")
                              ("r" "Question [inbox]" entry (file+headline "~/Documents/GTD/inbox.org" "Question") "* %i%? \n %U")
                              ("i" "Tickler"          entry (file+headline "~/Documents/GTD/tickler.org" "Tickler") "* %i%? \n %U")
                              ))

;; A suitable time stamp for log updates throughout a given day:
;; (format-time-string "*** %I:%M %p" (current-time))
;; which outputs:
;; "*** 02:23 PM"

;; Some of the ones used above:
;; %U [2022-12-19 Mon 12:59]
;; %u [2022-12-19 Mon]
;; %T <2022-12-19 Mon 12:59>
;; %t <2022-12-19 Mon>

;; They appear to be different data types (square brackets versus
;; angle brackets)... but the manual doesn't say anything. But they
;; may function that way?
;; See: https://orgmode.org/manual/Template-expansion.html

(defun sw-experimental-template ()
  "Return a string for org-capture-templates. Experimental."
  "* Hello sailor! %i%?"
  )

(sw-experimental-template)

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
                         "~/Documents/GTD/someday.org"
                         "~/Documents/GTD/tickler.org"
						 "~/Documents/GTD/reference.org"
                         ;;"~/Documents/GTD/minutes.org"
                         "~/.emacs.d/emacs.org"
						 ;;"~/wainstead/study-projects/subjects-and-progress.org"
                         )
      )

;; For use in refiling tasks from inbox.org (or anywhere probably)
;; maxlevel will search <= N for headings to file under
(setq org-refile-targets '(("~/Documents/GTD/gtd.org" :maxlevel . 1)
                           ("~/Documents/GTD/someday.org" :level . 1)
                           ("~/Documents/GTD/tickler.org" :maxlevel . 1)
                           ("~/Documents/GTD/minutes.org" :maxlevel . 1)
                           ("~/Documents/GTD/habits.org" :maxlevel . 1)
						   ("~/Documents/GTD/reference.org" :maxlevel . 1)
						   ("~/Documents/GTD/done.org" :maxlevel . 1)
						   ("~/Documents/GTD/backlog.org" :maxlevel . 1)
						   ("~/Documents/GTD/maybe.org" :maxlevel . 1)
						   ("~/.emacs.d/emacs.org" :maxlevel . 1)
						   ;;("~/wainstead/study-projects/subjects-and-progress.org" :regexp . "learning")
               ))

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "BLOCKED(b)" "QUESTION(k)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(r)" "OBSOLETE(o)" )))

(setq org-todo-keyword-faces
      '(("DEFERRED" . (:foreground "red" :weight bold))
		("STARTED" . "yellow")
		("WAITING" . "red")
		("BLOCKED" . "red")
		("QUESTION" . "blue")
		("NEXT" . "green")
        ("CANCELLED" . (:foreground "red" :weight bold))))

;; Items marked DONE get a timestamp. Note that CANCELLED, DEFERRED,
;; OBSOLETE also get the timestamp probably because of the structure
;; of org-todo-keywords. Just a guess.
(setq org-log-done 'time)

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
                                            "makefile"
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


;; TODO: fix "today" "today" as it's not correct, tho it works.
(defun sw-org-today ()
  "Show tasks tagged 'today'"
  (interactive)
(org-tags-view "today" "today")
)

;; Found here: https://emacs.stackexchange.com/a/17405
;; Create different colors for the priorities
(setq org-priority-faces '((?A . (:foreground "green" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "skyblue"))))
