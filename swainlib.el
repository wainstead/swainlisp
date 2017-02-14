;; hints:
;; rename-uniquely
;; rename-buffer is better though
;; send-invisible
;; C-u M-| to run command on region
;; C-u M-! to put in current buffer

;; C-c C-r in cmd mode jumps back to previous prompt, if you have a lot of output
;; C-c C-e to go back
;; C-c C-p, C-c C-n to jump between commands

;; C-x r m sets bookmark
;; C-x r b prompts for bookmark
;; C-x r l to get the list and use dir-style commands
;; all bookmark commands start with 'bookmark'

;; C-x v v is 'next action' in VC mode
;; vc-default-back-end

;; these only appear to work in picture/rectangle mode
;; C-c C-w \w for saving something to a register
;; C-c C-x \w for getting it back
;; string-rectangle will replace rectangle with string
;; C-x M M to access the command history of the minibuffer

;; registers!!!
;; M-x apropos register for a list of register commands, which allow you
;; to store points (marks) in registers and jump to them, and store text
;; in them like vi can.
;; C-x r SPACE to save a point to a register
;; C-x r j to jump to a point stored in a register
;; C-x r i insert contents of a register
;; C-x r x or s copy to register

;; M-m moves to first nonwhitespace char of current line
;; M-r moves cursor to center of window

;; Recursive edits:
;; C-r to edit, C-w to delete match and start custom edit, ESC C-c to continue
;; C-] exits recursive edit and query-replace
;; a comma replaces the instance to show you what it will look like, hit y or n
;; a carret will go back to the previous match
;; a period replaces the current instance and quits.

;; v t toggles terse display in vc-directory

;; delete-indentation is like 'J' in vi. Use M-^ to join current
;; line to previous, and C-u M-^ to join to next.

;; double space a region: C-u M-| sed G RET
;; delete ALL blank lines from a file (same as "grep '.' "): sed '/^$/d'

;; M-. to find tag, M-* to pop out; these are the defaults. M-. is
;; remapped to C-x-. You use C-u C-x-. to get to the next matching
;; tag. In tags-search it's C-x-, which is another candidate for 
;; a replacement command.

;; M-/ is dabbrev-expand, which searches all buffers for an expansion. Rules.

;; The oddly named finder-by-keyword is how you discover modes that are
;; available. Also f1-p.

;; C-x RET f undecided-unix RET to convert DOS files to Unix format.
;; However see the node "Text and Binary" in the Emacs manual (info) 
;; on how to tell Emacs whole dirs are a particular format.

;; Replacing text in multiple files: find-dired, mark the files, type Q

;; hi-lock is the mode that allows arbitrary highlighting of regexps.
;; try highlight-regexp.

;; Info-speedbar-browser gives you all the info nodes in a speedbar.

;; cperl-mode does a better job of syntax highlighting

;; M-x make-frame-on-display opens a frame on another machine, so two 
;; people can edit the same buffer.

;; show-paren-mode highlights matching delimiters like () {} [] etc.

;; changing faces:
;; Sorry for the empty other mail.  Try M-x customize-group RET ansi-color
;; RET.  There is a vector with the color names used somewhere in that
;; buffer.  Personally, I use cornflowerblue instead of blue.  :)
;; Alex. <alex@gnu.org>

;; vc-annotate labels the lines with the author's name; just do v g in
;; vc-dired mode.

;; To open a tag in a new window instead of C-x . do C-x 5 .

;; M C x when editing a shell script runs sh-execute-region

;; escaping from recursive edits: abort-recursive-edit

;; make a TAGS file for your install of Emacs plus the libraries locally
;; find /usr/local/share/emacs ~swain/.emacs.d \( -name \*.el -o -name .emacs \) | sudo etags -

;; make any old tags table:
;; find . -name "*.[chCH]" -print | etags -

;; compilation, for speedy loading:
;; emacs -batch -f batch-byte-compile *.el
;; byte-recompile-directory
;; byte-compile-file

;; term mode: http://www.bothner.com/~bothner/software/README.term
;; C-c C-l for line mode (like shell mode) and C-c C-k for char mode (xterm)


;; C-M-f goto closing brace (standing on the opening brace)
;; C-M-b goto opening brace (standing on the closing brace)

;; M-x set-goal-column

;; (global-set-key (kbd "<f2> w") 'keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end cheat sheet



(when window-system
  ;; make pretty
  (defvar default-frame-alist '((background-color . "#040000")
                                (foreground-color . "goldenrod")
                                (cursor-color     . "red")))
  "Default colors for Emacs.")

;; default mode gunk

;;(setq load-path (append load-path (list "~swain/.emacs.d")))

;; set the default command for M-x compile
(setq compile-command "make")
;;"cd .; ant -emacs -f build.xml targetName")

;; always scroll the buffer as compilation proceeds...
(setq compilation-scroll-output t)

;; enable paren matching
(show-paren-mode)

;;(set default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [(meta ??)] 'help) ;; finally

;; from a post on slashdot: how to do away with some of the gunk
;;(setq emacs22 (eq emacs-major-version 22))

;;(when emacs22
(blink-cursor-mode t)
;;(tool-bar-mode -1)
;;    (tooltip-mode -1)
;;)

;; allow ansi colors in shell mode, i.e. let ls --color=yes work right
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; always use font-lock-mode in modes that support it
(global-font-lock-mode t)

;; load the desktop on startup (from ~/)
(desktop-load-default)
;; automatically save the desktop on exit.
(setq desktop-enable t)
(load-file "~swain/.emacs.d/desktop-auto-save.el")
(load-file "~swain/.emacs.d/tail-logs.el")
(load-library "ibuffer")

;; fix isearch so we can use backspace instead of delete
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; show the column number so we don't exceed 80 columns when coding
(column-number-mode t)

;; fix tab width to something readable
;; this has never worked for some reason
(setq tab-width 4)

;; automatically load the .abbrev_defs file and abbrev-mode
(defvar sw-abbrev-defs
  "~swain/.abbrev_defs"
  "abbreviation definitions file")
;; first time caller? make the file
(if (not (file-exists-p sw-abbrev-defs))
    (shell-command (format "touch %s" sw-abbrev-defs))
  )
(setq-default abbrev-mode t)
(read-abbrev-file sw-abbrev-defs)
(setq save-abbrevs t)

;; hide passwords

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; clock in status bar
(setq display-time-day-and-date t)
(display-time)

;; start the Emacs server
(server-start)

;; death to Crisp mode!
(setq crisp-override-meta-x nil)

;; gnus config
(setq gnus-select-method '(nntp "news.panix.com"))

;; get the speedbar... or not...
(unless window-system
  ;;(speedbar) ;; we just never use it. alas.
  ;; else
  (menu-bar-mode nil))

;; check the man page for the 'date' command to format the day/time
;; differently..
(defun sw-display-seconds-in-status-bar ()
  "Make the clock display the seconds so I know when cron is going to run..."
  (interactive)
  (setq display-time-interval 1)
  (setq display-time-format "%c")
  (display-time)
  )

;; larnep asked for this, so it is here. All tabs are spaces.
(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "cornflowerblue" "magenta" "cyan" "white"])
 '(ibuffer-saved-limits (quote (("java" ((name . ".java"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(indent-tabs-mode nil)
 '(line-number-display-limit nil)
 '(scroll-conservatively 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(show-paren-match-face ((((class color)) (:background "navy" :foreground "yellow"))))
 '(tnt-other-name-face ((((class color)) (:foreground "skyblue")))))


;; scroll one line at a time

(defun sw-scroll-up-n (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-up (prefix-numeric-value n)))

(defun sw-scroll-down-n (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-down (prefix-numeric-value n)))

(global-set-key "\M-o" 'sw-scroll-up-n)
(global-set-key "\M-i" 'sw-scroll-down-n)

;; scroll left or right one column at a time
;; note the funky way the keyboard chars are encoded

(defun sw-scroll-left-n (&optional n)
  "Scroll left N lines (1 by default)."
  (interactive "P")
  (scroll-left (prefix-numeric-value n)))

(defun sw-scroll-right-n (&optional n)
  "Scroll right N lines (1 by default)."
  (interactive "P")
  (scroll-right (prefix-numeric-value n)))


;; I never use these
;; (global-set-key [(control ?.)] 'sw-scroll-left-n)
;; (global-set-key [(control ?,)] 'sw-scroll-right-n)


;; move the cursor to the top/bottom of the window
;; (defun point-to-top ()
;;   "Put the point on top line of the window."
;;   (interactive)
;;   (move-to-window-line 0))
;; (defun point-to-bottom ()
;;   "Put the point at the beginning of the last visible line."
;;   (interactive)
;;   (move-to-window-line -1))

;; (global-set-key "\M-," 'point-to-top)
;; (global-set-key "\M-." 'point-to-bottom)



;; These are reset as part of the example code in "Writing GNU Emacs
;; Extensions".
(global-set-key "\C-x," 'tags-loop-continue)
(global-set-key "\C-x." 'find-tag)


(defvar wipe-window-top (make-marker))
(defvar wipe-window-bot (make-marker))

(defun wipe-window ()
  "Erase all text visible in the window, joining the top and bottom"
  (interactive)
  (move-to-window-line 0)
  (set-marker wipe-window-top (point))
  (move-to-window-line -1)
  (end-of-line)
  (set-marker wipe-window-bot (point))
  (delete-region wipe-window-top wipe-window-bot)
  (kill-line))

;; Note that (interactive) can take arguments specifying the format
;; expected; the variables are specified in the function definition
;; line (sw-replace).

(defun sw-replace (find with)
  "Replace all occurances interactively from beginning of buffer"
  (interactive "sEnter search string: \nsEnter a replacement: ")
  (save-excursion
    (goto-char (point-min))
    (query-replace find with))
  )

;; bind goto-line to a key
(global-set-key "\M-n" 'goto-line)

(defun sw-shell (sw-buff-name)
  "A wrapper for M-x shell. Prompt for shell buffer name, and if
there are autosaved contents from a previous incarnation, insert
them."
  (interactive "sBuffer name: ")
  (let ( (the-buffer (get-buffer sw-buff-name)) )
    (if (bufferp the-buffer)
        (switch-to-buffer the-buffer)
      ;; else create it

      (shell)
      (rename-buffer sw-buff-name)
      ;;(make-local-variable 'buffer-contents-restored)
      ;;(setq buffer-contents-restored nil)
      (if (and sw-restore-shell-buffers-flag (file-exists-p (concat sw-buffer-file-name-prefix sw-buff-name)))
          (sw-insert-saved-buffer-contents sw-buff-name)
        (message "No auto-desktop-save file exists for this buffer, or sw-restore-shell-buffers-flag is nil")
        )
      )
    )
  )

(defun sw-cli ()
  "Open a shell buffer and name it \"cli\""
  (interactive)
  (sw-shell "cli")
  )


(defun sw-root ()
  "A named shell buffer for doing root stuff."
  (interactive)
  (sw-shell "root")
  )

(defun sw-sql ()
  "Open a shell buffer, rename it 'sql'"
  (interactive)
  (sw-shell "sql")
  (abbrev-mode t)
  (read-abbrev-file "~swain/.abbrev_defs")
  )

(defun sw-tail ()
  "Open a shell buffer, rename it 'tail' for tailing files"
  (interactive)
  (sw-shell "tail")
  )

(defun clr ()
  "git-commit all buffers, then clear the current buffer."
  (interactive)
  (sw-git-commit-buffers)
  (erase-buffer)
)

;; from the O'Reilly book on writing extensions
;; C-x b will not switch you to a buffer unless it exists
;; C-u C-x b will allow you to create new ones
;; does not work on spork for some reason, nor orbital
(defadvice switch-to-buffer (before existing-buffer
                                    activate compile)
  "When interactive, switch to existing buffers only, unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))

;; I'm tired of C-x C-b opening that buffer...
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(defun sw-qs () 
  "quickly switch between buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))

;; set M-a to the above function
(global-set-key "\M-a" 'sw-qs)

;; insert a javadoc comment
(fset 'insert-javadoc
      [?/ ?* ?* return ?* return ?* ?/ up ? ])

;; Here's a command to insert a new log entry in the format I made up
(defun sw-start-new-log-entry ()
  "Insert a row of hash marks and then the date in swain format"
  (interactive)
  (goto-char (point-max))
  (insert "\n\n\n#########################################################################\n")
  (insert (format-time-string "%y%m%d %c\n\n" (current-time))))

;; Insert the date and time in a sortable way 
(defun sw-insert-date ()
  "Insert the current date and time so I can track myself"
  (interactive)
  (goto-char (point-max))
  (insert (format-time-string "\n%y%m%d %T " (current-time))))

;; write out an Oracle TO_DATE() function. Date is set to the here and now.
(defun todate-oracle ()
  "Insert a TO_DATE() function for Oracle SQL"
  (interactive)
  (insert (format-time-string "TO_DATE('%c', 'Dy Mon DD HH24:MI:SS YYYY')" (current-time))))


;; allow sw-qs to work with java mode too
(add-hook 'java-mode-hook
          (lambda ()
            (define-key java-mode-map "\M-a" 'sw-qs)))

;; make n and p work without control key for buffers like *grep*
(add-hook 'compilation-mode-hook
          (lambda ()
            (define-key compilation-mode-map "n" 'next-line)
            (define-key compilation-mode-map "p" 'previous-line)))

;; double space the lines of a region, useful for grep output etc.
(fset 'double-space-region
      [?\C-u ?\M-| ?s ?e ?d ?  ?G return])

; fix the fookin bookmark mode. is there any consistency in the emacs world?
; tested, seems to work OK.
(eval-after-load "bookmark"
  '(progn
     (define-key bookmark-bmenu-mode-map "
" 'bookmark-bmenu-this-window)))

;; files to open in sgml-mode, java-mode, whatnot
(add-to-list 'auto-mode-alist '("\\.jhtml$"   . sgml-mode       ))
(add-to-list 'auto-mode-alist '("\\.jsp$"     . java-mode       ))
(add-to-list 'auto-mode-alist '("\\.bsh$"     . java-mode       ))
(add-to-list 'auto-mode-alist '("\\.inputs$"  . sh-mode         ))
(add-to-list 'auto-mode-alist '("\\.data$"    . sgml-mode       ))
(add-to-list 'auto-mode-alist '("\\.tld$"     . sgml-mode       ))
(add-to-list 'auto-mode-alist '("\\.vps$"     . python-mode     ))
(add-to-list 'auto-mode-alist '("\\.tt2$"     . html-mode       ))
(add-to-list 'auto-mode-alist '("\\.xsd$"     . sgml-mode       ))
(add-to-list 'auto-mode-alist '("\\.js$"      . js-mode ))
(add-to-list 'auto-mode-alist '("\\.xpi$"     . archive-mode    ))
(add-to-list 'auto-mode-alist '("\\.rb$"      . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.rby$"     . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.rhtml$"   . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.tpl$"     . php-mode        ))

;; thanks Jeff Dairiki for this hook
(defun my-php-mode-hook-func ()
  (c-set-style "gnu")
  (setq tab-width 4
        c-basic-offset 4
        c-hanging-comment-ender-p nil
        indent-tabs-mode nil))

(add-hook 'php-mode-hook 'my-php-mode-hook-func)

;; I don't remember adding this or why.
(put 'downcase-region 'disabled nil)

;; Function keys
(global-set-key (kbd "<f2> p") (lambda () (interactive) 'comint-previous-input))
(global-set-key [(f3)] 'clr)
(global-set-key [(f4)] 'next-error)
(global-set-key [f5] 'compile)
(global-set-key [f6] `toggle-buffer-full-filename)
;; copy region to the X clipboard
;;(global-set-key [f7] 'clipboard-kill-ring-save)
;; yank from same
;;(global-set-key [\C-f7] 'clipboard-yank)
;; snappy: macro + keybinding = hooha
(fset 'next-frickin-tag
      "\C-u\C-x.")
(global-set-key [f7] 'next-frickin-tag)
(global-set-key [f8] 'find-file-at-point)
(global-set-key [f9] `sw-list)
(global-set-key [(control f9)] 'sw-next-log)
;; highlight-regexp is an alias to a hi-lock command, set in hi-lock.
(global-set-key [f11] 'highlight-regexp)
(global-set-key [\C-f11] 'unhighlight-regexp)
(global-set-key [f12] 'toggle-truncate-lines)
(global-set-key [f13] `hs-hide-level)
(global-set-key [(control f13)] `hs-show-all)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [pause] `delete-other-windows)
(global-set-key [print] `other-window)
(global-set-key "\C-c\C-g" 'font-lock-fontify-buffer)

;; found via groups.google.com, somehow
(defun toggle-buffer-full-filename ()
  "Toggle the buffer's name between just the filename and the file's full path.
Preserve any <number> suffix that the buffer name may have
already.  Give error if buffer is not associated with a file."
  (interactive)
  (let* ((buff-name (buffer-name))
         (file-name (buffer-file-name))
         (buffer-index
          (if (string-match "<[0-9]+>$" buff-name)
              (match-string 0 buff-name) ""))
         (full-filename-with-index (concat file-name buffer-index))
         (just-filename-with-index (concat (file-name-nondirectory file-name) buffer-index)))
    (rename-buffer
     (cond ((string= buff-name just-filename-with-index) full-filename-with-index)
           (t just-filename-with-index)))))

(put 'erase-buffer 'disabled nil)

;; remap C-x # to C-x c, which is more mnumonic, and easier on the carpals.
(global-set-key "\C-xc" 'server-edit)

(fset 'reformat-code
      [?\C-x ?h ?\C-u ?\M-| ?p ?e ?r ?l ?  ?- ?n ?p ?e ?  ?' ?s ?/ ?^ ?[ ?  ?\\ ?t ?] ?+ ?/ ?/ ?' return ?\C-x ?h ?\C-\M-\\])


(defun sw-fb ()
  "Add a flowerbox to a Java file"
  (interactive)
  (let ( (projectdir (getenv "HOME")))
    ;; change the string here to the path off your $HOME
    (setq projectdir (concat projectdir "/projects/bluewire"))
    (insert
     (format "/** \n *\n */
" 
;; calculate the filename's path: full pathname minus projectdir
(substring (buffer-file-name) (+ 1 (length projectdir))))))
)

;; looks ugly, but generates a simple Exception class definition
(defun sw-except (name)
  "Insert a new Java Exception class based on user input"
  (interactive "sClass name: ")
  (insert (format "
/** 
  * @author $Author: swain $
  * @version $Id: swainlib.el,v 1.50 2007/11/13 16:29:59 swain Exp $
  * 
  */
class %s extends Exception {
    public %s() {}
    public %s(String msg) {
        super(msg);
    }
}" name name name))
  )


;; don't show me annotations unless I ask
(setq bookmark-automatically-show-annotations nil)
(setq bookmark-save-flag t)

(fset 'greperr
      [?\C-x ?h ?\C-u ?\M-| ?g ?r ?e ?p ?  ?- ?i ?  ?e ?r ?r ?o ?r return])

;; fix shell mode
(add-hook 'shell-mode-hook 
          (lambda ()
            (setq comint-last-output-start (point-min-marker))))


(global-set-key [(control ?0)] 'unexpand-abbrev)

;; found this on http://www.emacswiki.org/cgi-bin/wiki.pl?HtmlEndOfLine
(defun html-end-of-line ()
  "If there is an HTML tag at the end of the line, then go to start of tag.
    Otherwise go to the real end of the line."
  (interactive)
  (if (or (looking-at ".*>$") ; if we're on a line that ends with a tag
          (and (= (char-before) 62)
               (= (point) (save-excursion
                            (end-of-line)
                            (point))))) ; or we're at the end of a line
                                        ; with a tag
      (let ((where-now (point)))
        (narrow-to-region
         (save-excursion
           (beginning-of-line)
           (point))
         (save-excursion
           (end-of-line)
           (point)))
        (end-of-line)
        (re-search-backward "<" nil t)
        (if (= (point) where-now)
            (end-of-line))
        (widen))
    (end-of-line)))

;; Send a random line from Zippy to the minibuffer.
(defun yow-ovrwt ()
  "Send a Zippyism to the minibuffer."
  (interactive)
  (message (yow)))

;; disable insert key, which sends us into overwrite mode.
(global-set-key [insert] `yow-ovrwt)

;; note that with the prefix argument the function "yow" does the same
;; thing
(defun yow-insert ()
  "Insert a random line from Zippy at point"
  (interactive)
  (insert (yow)))



;; prepare the instant messenger!! move all zig!!
;;(setq load-path (cons "~swain/.emacs.d/tnt" load-path))
;;(setq load-path (append load-path (list "~swain/.emacs.d/tnt")))
;;(load "tnt")
;;(setq tnt-use-timestamps t)

;; prompt user for a background color; foreground and cursor colors
;; are hardwired.
(defun sw-colors (color-string)
  "reset the colors for this frame"
  (interactive "sGive me a color (ex. '0A0000'): ")
  (if (not (= (length color-string) 6))
      (setq color-string "#050000")
    (setq color-string (format "#%s" color-string)))
  ;;  (set-foreground-color "wheat")
  (set-background-color color-string))
;;  (set-cursor-color "green"))

;; what it says
(fset 'view-kill-ring
      [?\M-x ?h ?e ?l ?p return ?v ?k ?i ?l ?l ?- ?r ?i ?n ?g return])

;; set up stuff for the sql-mysql mode
(setq sql-mysql-program "/usr/local/mysql/bin/mysql")
(setq sql-user "root")
(setq sql-database "v3")

;; Copied from ProjectBuilder's release notes for the December 2002
;; devtools update. But some files need to be copied over from the
;; 21.1 build that Apples ships with OS X.
;; (autoload 'gnuserv-start "gnuserv-compat" 
;;   "Allow this Emacs process to be a server for client processes." t)
;; (gnuserv-start)

;; Try to set the history list for hi-lock (formerly p-whim-lock)
(defvar hi-lock-face-history
  (list "hi-black-b" "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb"
        "hi-yellow" "hi-pink" "hi-green" "hi-blue" )
  "History list of faces for hi-lock interactive functions.")

;; original hack
;; (defvar p-whim-lock-face-history
;;       (list "bwl-liteblu" "bwl-pink" "bwl-red" "bwl-green" "bwl-blue" 
;; 	    "hi-yellow" "hi-blue" "hi-pink" "hi-green" "bwl-black")
;;       "History list of faces for whim-lock interactive functions.")

;; load this library for the functions that use browse-url-interactive-arg
;;(load-library "browse-url")

;; use compile to check the perl file in the current buffer
(defun sw-perl-wc ()
  (interactive)
  (let ( 
        (compile-command (format "perl -wc %s 1>&2" buffer-file-name)) )
    (compile compile-command)))

;; prompt user for x,y coordinates; move the pointer there
(defun sw-coord (x y)
  "Move cursor to the x,y coordinates provided by user"
  (interactive "nX coordinate: \nnY coordinate: ")
  (goto-line x)
  (forward-char y))

;; different buffer switcher. Solaris build complains about keyboard
;; bell with macros. Also handles case where ibuffer not created yet.
(defun sw-list ()
  "switch to Ibuffer; this function should be bound to F9"
  (interactive)
  (setq buffer (get-buffer "*Ibuffer*"))
  (if (bufferp buffer)
      (progn 
        (switch-to-buffer buffer)
        (ibuffer-update nil))
    (ibuffer))
  (delete-other-windows))

;; move my shell buffer "buffername" to the dir the file is in. if not
;; visiting file make named shell buffer

(defun sw-move-shell-here (buffername)
  "Move the cli buffer to the directory where the currently visited file is located,
 or prompt user if the buffer is not visiting the file ."
  (interactive)
  (let ( 
        (wanted-buffer (get-buffer buffername))
        (this-buffer-file default-directory)
        )
    (if (bufferp wanted-buffer)
        (switch-to-buffer wanted-buffer)
      ;; else make the shell buffer called "buffername"
      (let ( 
            (shell-buffer (get-buffer "*shell*"))
            )
        (if (not (bufferp shell-buffer))
            (shell)
          )
        (switch-to-buffer "*shell*")
        (rename-buffer buffername)
        )
      )
    ;; make sure we're at point-max, insert and send input
    (goto-char (point-max))
    (insert (format "cd %s" (file-name-directory this-buffer-file)))
    (comint-send-input)
    ))

(defun sw-move-cli-here ()
  "move the buffer named cli to the current directory"
  (interactive)
  (sw-move-shell-here "cli")
  )

(defun sw-move-root-here ()
  "move the buffer named root to the current directory"
  (interactive)
  (sw-move-shell-here "root")
  )

(defun sw-move-www-here ()
  "move the buffer named www to the current directory"
  (interactive)
  (sw-move-shell-here "www")
  )


(defun sw-delete-to-end ()
  "Delete all chars from point to end of buffer. Save the stuff in
the kill ring."
  (interactive)
  (delete-char (- (point-max) (point)) t))

;; needs work. should use commenting style of current major mode.

;; optimization: use (if) on (null current-prefix-arg) and assign a
;; symbol to point to the function you want to use (akin to a function
;; pointer).

(defvar sw-comment-pattern nil
  "Pattern to match when commenting out lines in sw-comment-lines")
(defvar sw-comment-chars "#"
  "What char is used to comment out lines in sw-comment-lines")
(defvar sw-already-commented-lines nil
  "Set to t when lines have been commented via sw-comment lines. Defaults to nil.")
(make-variable-buffer-local 'sw-comment-pattern)
(make-variable-buffer-local 'sw-comment-chars)
(make-variable-buffer-local 'sw-already-commented-lines)

(defun sw-comment-lines (pattern comment-chars)
  "Comment all lines in the buffer that match regexp."
  (interactive "sComment lines matching regexp: \nsPattern: ")
  (setq sw-comment-pattern pattern)
  (setq sw-comment-chars comment-chars)
  (setq sw-already-commented-lines t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern (point-max) t)
      (beginning-of-line)
      (insert (format "%s " comment-chars))
      (end-of-line)
      )
    )
  )

;; FIXME: undone
(defun sw-toggle-lines ()
  "Undo or redo the commenting of lines in the current buffer.
   Complains if you haven't run sw-comment-lines yet."
  (interactive)
  (if (null sw-already-commented-lines)
      (error "Call sw-comment-lines first.")
    (message (format "Teach me how to re-replace from ^ with '%s' and '%s'" sw-comment-pattern sw-comment-chars))
    )
  )

(defun sw-unix-to-human ()
  "Convert the Unix timestamp at point to human readable form."
  (interactive)
  (let ( (sw-unixtime (thing-at-point 'word)) )
    (if  (null sw-unixtime) 
        (error "Point does not appear to be on a Unix timestamp"))
    ;; oops. this always evaluates to true. must check that sw-unixtime contains only numbers.
    ;;  (if (not (numberp sw-unixtime))
    ;;  (error "Point does not appear to be on a Unix timestamp"))
    (shell-command (format "perl -e 'print scalar(localtime(%s))'" sw-unixtime))
    )
  )

;; replace ^M in an entire buffer. Needs to be reworked to do only a
;; region, eventually.
(fset 'sw-replace-M
      [?\M-< escape ?% ?\C-q ?\C-m return ?\C-q ?\C-j return ?!])

;; hacked this up for ronnyg
(defun multiyank (numtimes)
  "Yank the last thing in the kill ring as many times as the user
specifies. Also, if the prefix argument is given, insert a newline 
after each yank."
  (interactive "nNumber of times to yank: ")
  ;;(message (format "got %d" numtimes))
  (while (> numtimes 0)
    (yank)
    (setq numtimes (- numtimes 1))
    (if (not (null current-prefix-arg))
        (insert "\n"))
    )
  )


(defun perldoc (man-args)
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man man-args)))

(defun pydoc (man-args)
  (interactive "sPydoc: ")
  (require 'man)
  (let ((manual-program "pydoc"))
    (man man-args)))

(defun ri (man-args)
  (interactive "sri: ")
  (require 'man)
  (let ((manual-program "ri"))
    (man man-args)))


;; tested. works. yay.
(defun sw-clumpkill ()
  "Clump all kills together. You can then do one yank to get them all back."
  (interactive)
  (append-next-kill)
  (kill-line)
  )

(global-set-key "\M-k" 'sw-clumpkill) ;; m'kay?


(defun sw-grepv (pattern)
  "Do a grep -v on the fracking buffer."
  (interactive "sPattern: ")
  (shell-command-on-region (point-min) (point-max) (format "grep -v '%s'" pattern) 1 1 "*error crap*")
  )

(defun sw-php-lint ()
  "Run a lint check on the file the current buffer is visiting."
  (interactive)
  (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
    (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
    )
  )

(defun sw-chmod-plusx ()
  "Change the current file to executable."
  (interactive)
  (shell-command (format "chmod a+x %s" (buffer-file-name)))
)

;; prototype Expect functionality. Long, long ways to go on this one.

(defvar swain-regexp "foo" "This is a documentation string.")

;; this would need to be defined as a lambda function, or a closure,
;; which would be input to the expect-string function, which in turn
;; is added to the comint-output-filter-functions, but only locally to
;; a given buffer.

(defun swain-watch-for-stuff (string)
  "Copied from `comint-watch-for-password-prompt'. This function should be in the list `comint-output-filter-functions'."
  (when (string-match swain-regexp string)
    ;; perform action when output is matched
    (message "I see the foo.")))

(defun expect-string (pattern response)
  "Given a pattern, watch the output of this shell buffer and insert response when it's detected."
  (interactive "sPattern: \nsResponse: ")
  (message (format "%s and %s" pattern response))
  ;;(add-hook 'comint-output-filter-functions (lambda (string) (when (string-match pattern string) (message response))))
)

(add-hook 'comint-output-filter-functions 'swain-watch-for-stuff)

(fset 'sw-php-lint-check-on-buffer
      [?\C-x ?h ?\M-| ?p ?h ?p ?  ?- ?l return])

;; stolen from:
;; http://www.splode.com/~friedman/software/emacs-lisp/src/buffer-fns.el
;; this is a utility function used elsewherei
(defun apply-on-rectangle-region-points (fun beg end &rest args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns."
  (apply-on-rectangle
   (lambda (bcol ecol)
     (apply fun
            (progn
              (move-to-column bcol 'coerce)
              (point))
            (progn
              (move-to-column ecol 'coerce)
              (prog1
                  (point)
                (beginning-of-line)))
            args))
   beg end))


(defun downcase-rectangle (beg end)
  "Convert the marked rectangle to lower case."
  (interactive "r")
  (apply-on-rectangle-region-points 'downcase-region beg end))


(defun upcase-rectangle (beg end)
  "Convert the marked rectangle to upper case."
  (interactive "r")
  (apply-on-rectangle-region-points 'upcase-region beg end))

(defun sw-xml-prettyprint ()
  "Pretty print the farking ugly xml file we're looking at"
  (interactive)
  (shell-command (format "xmllint --format %s" (buffer-file-name)))
  (switch-to-buffer (get-buffer "*Shell Command Output*"))
  (xml-mode)
  (delete-other-windows)
  (toggle-read-only)
  )

(defun sw-xml-prettyprint-region ()
  "Pretty print the farking ugly xml file we're looking at"
  (interactive)
  (shell-command (format "xmllint --format --nowarning -" ))
  (switch-to-buffer (get-buffer "*Shell Command Output*"))
  (xml-mode)
  (delete-other-windows)
  (toggle-read-only)
  )

(fset 'sw-xml-format-region
   [?\C-u ?\M-| ?x ?m ?l ?l ?i ?n ?g backspace ?t ?  ?- ?- ?f ?o ?r ?m ?a ?t ?  ?- ?- ?n ?o ?a ?r backspace backspace ?w ?a ?r ?n ?i ?n ?g ?  ?- return])

;; for shell buffers, truncate them when they get too big
;;(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; variable by which to truncate them: comint-buffer-maximum-size to
;; set this locally use M-x make-variable-buffer-local, then set the
;; variable to the size you want for that buffer. Otherwise this is
;; set in .emacs-custom.el, and is handled by Emacs's customization
;; interface.

;; Give diff and patch files a color scheme that works with a black
;; background. Note the hyphen at the end of the basename: diff-mode-
(load-file "~swain/.emacs.d/diff-mode-.el")

;; One function and two convenience commands for running git diff and
;; putting the results in a special window.
(defun sw-git-diff-meta (output-buffer-name git-command)
  "Run git-command as a shell command; output to
   output-buffer-name."
  (switch-to-buffer (get-buffer-create output-buffer-name))
  (diff-mode)
  (shell-command git-command output-buffer-name)
  (hi-lock-unface-buffer "^diff.*")
  (hi-lock-face-buffer "^diff.*" "hi-green")
  (toggle-read-only)
)
(defun sw-git-diff ()
  "Run git diff, output to new buffer"
  (interactive) 
  (sw-git-diff-meta "*git diff*" "git diff")
  )

(defun sw-git-diff-master ()
  "Run git diff master HEAD, output to new buffer"
  (interactive)
  (sw-git-diff-meta "*git diff master*" "git diff master HEAD")
  )

;; What would also be useful: a function to pull out and list all the
;; currently active highlights in a given buffer. hi-lock probably has
;; a variable (maybe even an alist) storing this information. That way
;; I can iteratively highlight a buffer and when I've reached a point
;; of highest usefulness pull it all out... maybe even store it in a
;; register or a global so reloading the file just automagically does
;; the right thing.
(defun sw-highlight-stuff ()
  "Pass an alist to the recursive function sw-apply-hs-regexps to
   highlight stuff in the current buffer. This function is the UI
   to the recursive function sw-apply-hs-regexps."
  (interactive)
  (sw-apply-hs-regexps '(
                         ("EXCEPTION:" . "hi-red-b")
                         ("WARNING:" . "hi-yellow")
                         ("---> Report ran successfully." . "hi-green-b")
                         ))
  )

(defun sw-apply-hs-regexps (sw-hi-alist)
  "Recurse through an alist of (regexp . color) and use
hi-lock-face-buffer to activate each in the current buffer."
  (if sw-hi-alist
      (progn
        ;; first unhighlight it, in case it was already done; this
        ;; happens sometimes when Emacs asks us to reload a file
        ;; because it changed on disc. hi-lock-unface-buffer just
        ;; returns 'nil' if it wasn't faced already.
        (hi-lock-unface-buffer (car (car sw-hi-alist)))
        (hi-lock-face-buffer (car (car sw-hi-alist)) (cdr (car sw-hi-alist)))
        (sw-apply-hs-regexps (cdr sw-hi-alist))
        )
    )
  )

;; courtesy Dale Sedivec
;; originally titled my:yank-rectangle-to-new-lines
(defun sw-yank-rectangle-to-new-lines ()
  (interactive)
  (save-excursion (newline (length killed-rectangle)))
  (yank-rectangle))


;; load a newer version of org-mode
(setq load-path (cons "~/Dropbox/Applications/org-8.2.4/lisp" load-path))
(setq load-path (cons "~/Dropbox/Applications/org-8.2.4/contrib/lisp" load-path))

;; capture tasks easily
(setq org-default-notes-file "~swain/Dropbox/projects/GTD/notes.org")
(define-key global-map "\C-cc" 'org-capture)

(defun sw-git-show-sha-at-point ()
  "Run git-show on the SHA at point as a shell command."
  (interactive)
  (let ( (git-sha (thing-at-point 'word)) )
    (if  (null git-sha) 
        (error "Point does not appear to be on a Unix timestamp"))
    (shell-command (format "git show %s" git-sha))
    )
  )

(load-file "~swain/.emacs.d/python-settings.el")

;; New code from Dale: toggle full screen mode. Light edits from
;; original.

(defun sw-toggle-full-screen (&optional frame)
  (interactive)
  (set-frame-parameter frame 'fullscreen
                       (if (frame-parameter frame 'fullscreen) nil 'fullboth)))


;; usage: type in your sql statement, then type \C-x. This wraps the
;; query in \x commands.
(fset 'psql-expand-output
   [?\C-a ?\C-k ?\\ ?x return ?\C-y return ?\\ ?x return])
;; need to add this via a hook function though
;; (define-key sql-mode-map (kbd "x") 'psql-expand-output)

;; Define my personal keymap, whose prefix key will be control-;
;; (next step is to define my own minor mode to override major modes
;; (quickswitch, for one))
;; improve this: define my own minor mode to reduce typing! This
;; keymap can then be the minor mode's keymap.
(defvar sw-map nil
  "Steve Wainstead's personal keymap for any mode")
(define-prefix-command 'sw-map)
(global-set-key [(control ?;)] 'sw-map)
;; set my quickswitch macro to 'a'
(define-key sw-map (kbd "a") 'sw-qs)

;;(define-key comint-mode-map [(meta-prior)] 'comint-previous-prompt)

;; Perhaps these should be in a personal minor mode. The key bindings
;; were originally set to ones from Writing GNU Emacs Extensions, and
;; they scrolled the screen left or right. I never used them in the 16
;; years I had them. Due to RSIs I needed an easier way to switch
;; windows than C-xo. Emacs made me use next-multiframe-window prior
;; to this; that kinda sucked because I scrolled through all windows
;; in all frames, which was not desirable.
(defun sw-next-window ()
  (interactive)
  (other-window 1))
(defun sw-previous-window ()
  (interactive)
  (other-window -1))
(global-set-key "\M-," 'sw-next-window)
(global-set-key "\M-." 'sw-previous-window)
(global-set-key [\C-prior] 'comint-previous-prompt)
(global-set-key [\C-next] 'comint-next-prompt)


;; it ain't bound to anything, so bind it and save my pinkies another
;; trip
(global-set-key [(control ?`)] 'next-error)


;; key-chord stuff
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "cl" 'sw-cli)
(key-chord-define-global "df" 'next-buffer)
(key-chord-define-global "j1" 'delete-other-windows)
(key-chord-define-global "ps" '(lambda () (interactive) (switch-to-buffer (get-buffer "psql"))))
(key-chord-define-global "./" 'undo)
