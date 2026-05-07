(add-to-list 'load-path "~/.emacs.d/personal-lisp")
;; Load org-mode-mods from GTD repository
(add-to-list 'load-path "~/Documents/GTD")
(load "org-mode-mods")

;; Load modular package declarations
(load "packages-langs")
(load "packages-tools")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; god-mode stuff

(defun god-mode-enabled-hook ()
  "Hook function to run when god-mode enabled"
  ;;(message "God mode enabled")
  (setq-local face-remapping-alist '((default . (:foreground "goldenrod" :background "#202030"))))
  )
(defun god-mode-disabled-hook ()
  "Hook function to run when disabling god-mode"
  ;;(message "God mode disabled")
  (kill-local-variable 'face-remapping-alist)
  ;;(setq-local face-remapping-alist '((default . (:foreground "goldenrod" :background "black"))))
  )
(add-hook 'god-mode-enabled-hook 'god-mode-enabled-hook)
(add-hook 'god-mode-disabled-hook 'god-mode-disabled-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end god-mode stuff

(when window-system
  (add-to-list 'default-frame-alist '(foreground-color . "goldenrod"))
  (add-to-list 'default-frame-alist '(background-color . "black"))
  "Custom colors for Emacs.")

;; always scroll the buffer as compilation proceeds...
(setq compilation-scroll-output t)

;; enable paren matching
(show-paren-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (global-set-key [(meta ??)] 'help) ;; finally
;; (global-unset-key [(meta ??)])


(blink-cursor-mode t)

;; allow ansi colors in shell mode, i.e. let ls --color=yes work right
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; always use font-lock-mode in modes that support it
(global-font-lock-mode t)

;; automatically save the desktop on exit.
(setq desktop-save-mode t)
(load "desktop-auto-save")
(load "tail-logs.el")
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
  "~/.abbrev_defs"
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

(if window-system
    ;; disable these things
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  ;;(toggle-scroll-bar t)

  )

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

;; Haven't been bound to keys in ages
;; Would have been super useful in csv-mode
;; (global-set-key "\C-x," 'sw-scroll-left-n)
;; (global-set-key "\C-x." 'sw-scroll-right-n)

;; Use these for symbol searches (i.e. TAGS file)
(global-set-key "\C-x," 'xref-pop-marker-stack)
(global-set-key "\C-x." 'xref-find-definitions)


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
  (read-abbrev-file "~/.abbrev_defs")
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

(defun sw-start-new-log-entry ()
  "Insert a new (org-mode) node for the day's entries"
  (interactive)
  (goto-char (point-max))
  ;; This line is still visually useful for marking the end of an
  ;; entry and the start of the next.
  (insert "\n#########################################################################\n")
  (insert (format-time-string "*** %y%m%d %c\n\n" (current-time))))

;; make n and p work without control key for buffers like *grep*
;; h/t to Dale Sedivec for suggesting modifying the parent mode's map!
(add-hook 'special-mode-hook
          (lambda ()
            (define-key special-mode-map "n" 'next-line)
            (define-key special-mode-map "p" 'previous-line)))

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
(add-to-list 'auto-mode-alist '("\\.js$"      . js-mode         ))
(add-to-list 'auto-mode-alist '("\\.xpi$"     . archive-mode    ))
(add-to-list 'auto-mode-alist '("\\.rb$"      . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.rby$"     . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.rhtml$"   . ruby-mode       ))
(add-to-list 'auto-mode-alist '("\\.tsx$"     . typescript-mode ))

;; highlight-indentation-mode is pretty essential for these major modes
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'json-mode-hook 'highlight-indentation-mode)

(put 'downcase-region 'disabled nil)

;; snappy: macro + keybinding = hooha
(fset 'next-frickin-tag
      "\C-u\C-x.")

;; highlight-regexp is an alias to a hi-lock command, set in hi-lock.
;; I now use "M-s h r" because it's in easy reach, and there's other
;; cool things under the "M-s h" prefix.
(global-set-key [f12] 'toggle-truncate-lines)
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

;; don't show me annotations unless I ask
(setq bookmark-automatically-show-annotations nil)
(setq bookmark-save-flag t)

;; fix shell mode
(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-last-output-start (point-min-marker))))

(global-set-key [(control ?0)] 'unexpand-abbrev)


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


;; Try to set the history list for hi-lock (formerly p-whim-lock)
(defvar hi-lock-face-history
  (list "hi-black-b" "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb"
        "hi-yellow" "hi-pink" "hi-green" "hi-blue" )
  "History list of faces for hi-lock interactive functions.")

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
  "switch to Ibuffer"
  (interactive)
  (setq buffer (get-buffer "*Ibuffer*"))
  (if (bufferp buffer)
      (progn
        (switch-to-buffer buffer)
        (ibuffer-update nil))
    (ibuffer))
  ;;(delete-other-windows)
  )

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

(defun sw-delete-to-end ()
  "Delete all chars from point to end of buffer. Save the stuff in
the kill ring."
  (interactive)
  (delete-char (- (point-max) (point)) t))

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

;; 2024 reimplementation: convert Unix timestamp to human food, but
;; this assumes thing-at-point is a number, unlike the previous
;; function. Also, the implementation is totally native Emacs Lisp.

(defun sw-convert-unix-timestamp ()
  "Convert the Unix timestamp at point to a human-readable format"
  (interactive)
  (message
   (format-time-string "%Y-%m-%d %a %H:%M:%S"
					   (seconds-to-time (thing-at-point 'number)))))

 (require 'man)
(defun perldoc (man-args)
  (interactive "sPerldoc: ")
  (let ((manual-program "perldoc"))
    (man man-args)))

(defun pydoc (man-args)
  (interactive "sPydoc: ")
  (let ((manual-program "pydoc"))
    (man man-args)))

(defun sw-grepv (pattern)
  "Do a grep -v on the fracking buffer."
  (interactive "sPattern: ")
  (shell-command-on-region (point-min) (point-max) (format "grep -v '%s'" pattern) 1 1 "*error crap*")
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

;;(load-file "~/.emacs.d/diff-mode-.el")

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

;; courtesy Dale Sedivec
;; originally titled my:yank-rectangle-to-new-lines
(defun sw-yank-rectangle-to-new-lines ()
  (interactive)
  (save-excursion (newline (length killed-rectangle)))
  (yank-rectangle))

(defun sw-git-show-sha-at-point ()
  "Run git-show on the SHA at point as a shell command."
  (interactive)
  (let ( (git-sha (thing-at-point 'word)) )
    (if  (null git-sha)
        (error "Point does not appear to be on a Unix timestamp"))
    (shell-command
     (format "git show %s" git-sha) (format "git show %s" git-sha))
    )
  )

;; New code from Dale: toggle full screen mode. Light edits from
;; original.

(defun sw-toggle-full-screen (&optional frame)
  (interactive)
  (set-frame-parameter frame 'fullscreen
                       (if (frame-parameter frame 'fullscreen) nil 'fullboth)))


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

(defun sw-intermodal-save-buffer ()
  "Save the buffer:
1. If it's a shell buffer, write its contents out silently
2. If it's a file, save the file
3. If it's neither, beep and complain"
  (interactive)
  (if (derived-mode-p 'comint-mode)
      (sw-save-buffer-invisibly (current-buffer))
    ;; else
    (if buffer-file-name
	(save-buffer)
      (beep)
      (message "Buffer is not visiting a file"))))




;; key-chord stuff
;; (loaded by packages-core.el)
(key-chord-mode 1)
(key-chord-define-global "jv" 'next-buffer)
(key-chord-define-global "j1" 'delete-other-windows)
(key-chord-define-global "j2" 'split-window-below)
(key-chord-define-global "j3" 'split-window-right)
(key-chord-define-global "jf" 'switch-to-buffer)
(key-chord-define-global "f0" 'delete-window)
(key-chord-define-global "jl" 'other-window)

(key-chord-define-global "jq" '(lambda () (interactive) (switch-to-buffer (get-buffer "*SQL*"))))
;;(key-chord-define-global "JQ" '(lambda () (interactive) (switch-to-buffer (get-buffer "psql"))))

(key-chord-define-global "pf" '(lambda () (interactive) (switch-to-buffer (get-buffer "*Python*"))))
;;(key-chord-define-global "PF" '(lambda () (interactive) (switch-to-buffer (get-buffer "*Python*"))))

(key-chord-define-global "jk" 'sw-qs)
;;(key-chord-define-global "JK" 'sw-qs) ;; note it's the caps lock version of the previous

(key-chord-define-global "jc" 'sw-cli)
;;(key-chord-define-global "JC" 'sw-cli)

(key-chord-define-global "js" 'sw-intermodal-save-buffer)
;; Cannot turn off caps lock from Emacs, alas... so just complain
(key-chord-define-global "JS"
			 '(lambda ()
			    (interactive)
			    (beep)
			    (message "Nope, caps lock key engaged")))

(key-chord-define-global "jw" 'other-frame)
;;(key-chord-define-global "JW" 'other-frame)
;; note that 'jx' is now reserved as the prefix code for my keymap
;; sw-jx-map.

;; Bringing this back since I keep hitting it anyway.
(global-set-key "\M-`" 'other-frame)

;; new prompt parsing for shells
(set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)]" 1 nil))
(dirtrack-mode)

(define-key key-translation-map [(meta ? )] [(control ?x)])

;; auto save file: do so after 100 chars typed, default is 300
;; https://www.emacswiki.org/emacs/AutoSave
(setq auto-save-interval 100)

;; good bye saving files with \C-x \C-s, a killer of the left hand.
(defun save-the-other-way-steve ()
  "Don't use C-xC-s anymore, use 'js'."
  (interactive)
  (ding)
  (display-warning :error "Use key-chord 'js' for saving files now"))
(global-set-key "\C-x\C-s" 'save-the-other-way-steve)

;; faster way to pop out now, I suppose: \C-x ,
(defun pop-out-the-other-way-steve ()
  "Don't use M-* anymore, use '\C-x ,'."
  (interactive)
  (ding)
  (display-warning :error "Use '\C-x ,' for popping out now"))
(global-set-key [(meta ?*)] 'pop-out-the-other-way-steve)

;; and, forevermore, forevermore...
(define-key key-translation-map [(meta ? )] [(control ?x)])
;;(define-key key-translation-map [(meta ? )(meta ?c)] [(control ?c)])

;; Define my personal keymap, whose prefix key will be control-;
;; (UPDATE: now using jx via key-chord.el) (next step is to define my
;; own minor mode to override major modes (quickswitch, for one))
;; NOTE: Dale recommends god-mode as well, which will probabaly do
;; what I want: improve this: define my own minor mode to reduce
;; typing! This keymap can then be the minor mode's keymap.
(defvar sw-jx-map nil
  "Steve Wainstead's first personal keymap for any mode.")

;; for jx
(define-prefix-command 'sw-jx-map)
(key-chord-define-global "jx" sw-jx-map)

;; god-mode, local buffer, keybinding
(key-chord-define-global "jg" 'god-local-mode)

;; for jz
(define-prefix-command 'sw-jz-map)
(key-chord-define-global "jz" sw-jz-map)


(define-key sw-jz-map (kbd "o") 'comint-show-output)

(defvar sw-zx-map nil
  "Another of Steve Wainstead's personal keymaps for any mode.")
(defun hello-sailor () "test function" (interactive) (message "hello, sailor!"))
(define-prefix-command 'sw-zx-map)
(key-chord-define-global "zx" sw-zx-map)
(define-key sw-zx-map (kbd "l") 'hello-sailor)

(global-set-key [(control ?;)] 'sw-jx-map)

;; set my quickswitch macro to 'a'
(define-key sw-jx-map (kbd "a") 'sw-qs)

;; thank you https://stackoverflow.com/questions/25473660/how-do-i-use-a-key-chord-combination-as-a-prefix-binding
;;(key-chord-define-global "jx" sw-map)
(define-key sw-jx-map (kbd "t") 'org-todo)
(define-key sw-jx-map (kbd "m") 'compile)
(define-key sw-jx-map (kbd "u") 'winner-undo)
(define-key sw-jx-map (kbd "r") 'winner-redo)


;; Dale told me this: make the Option key send 'alt'
(setq ns-option-modifier 'alt)

(defvar sw-magit-map nil "Steve's own keymap for magit commands.")
(define-prefix-command 'sw-magit-map)
(global-set-key "\M-sm" 'sw-magit-map)
(define-key sw-magit-map (kbd "s") 'magit-status)

(add-hook 'comint-mode-hook
          (lambda ()
	    (define-key comint-mode-map "\M-P" 'comint-previous-prompt)
	    (define-key comint-mode-map "\M-N" 'comint-next-prompt)))
(add-hook 'comint-mode-hook 'dirtrack-mode)

(global-set-key "\M-ss" 'isearch-forward)
(global-set-key "\M-sr" 'isearch-backward)
(global-set-key "\M-sc" 'clr)

;; I open my cheatsheet so often I may as well bind it
(defun sw-open-cheatsheet ()
  "open my cheatsheet"
  (interactive)
  (find-file "~/Documents/work-journal/cheatsheet.org")
  )
(define-key sw-jx-map (kbd "c") 'sw-open-cheatsheet)
(define-key sw-jx-map (kbd "o") 'comint-delete-output)

;; filched from http://nullprogram.com/blog/2010/10/06/
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun sw-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(define-key sw-jx-map (kbd "8") 'set-80-columns)
(define-key sw-jx-map (kbd "i") 'sw-list)
(define-key sw-jx-map (kbd "f") 'find-file-at-point)

(define-key sw-jx-map [f3] 'clr)
(define-key sw-jx-map [f5] 'compile)
(define-key sw-jx-map [f6] `toggle-buffer-full-filename)
(define-key sw-jx-map (kbd "%") `query-replace)
;; does not work. hrmm.

(defvar sw-meta-a-map nil
  "Steve Wainstead's personal keymap bound to M-a.")
(define-prefix-command 'sw-meta-a-map)
(global-set-key "\M-a" 'sw-meta-a-map)
(define-key sw-meta-a-map "d" 'delete-trailing-whitespace)
(define-key sw-meta-a-map "f" 'find-file)
(define-key sw-meta-a-map "\M-f" 'find-file-at-point)
(define-key sw-meta-a-map "u" 'untabify)
(define-key sw-meta-a-map "o" 'comint-delete-output)

;; Now we can load this, hopefully
(load "sw-sql-map")
;; Compilation stuff
(defvar sw-compile-map nil
  "Steve Wainstead's personal keymap for compilation commands")
(define-prefix-command 'sw-compile-map)
(global-set-key "\M-ac" 'sw-compile-map)

(global-set-key "\M-h" 'ns-do-hide-emacs)

;; bind uppercase equivalents to vim cursor movement keys
(global-set-key [(meta ?J)] 'next-line)
(global-set-key [(meta ?K)] 'previous-line)
(global-set-key [(meta ?H)] 'backward-char)
(global-set-key [(meta ?L)] 'forward-char)

;; or use alt, because it's not used for practically anything!
(global-set-key [(alt ?j)] 'next-line)
(global-set-key [(alt ?k)] 'previous-line)
(global-set-key [(alt ?h)] 'backward-char)
(global-set-key [(alt ?l)] 'forward-char)

;;(global-set-key (kbd "M-j") 'forward-char)

(define-key sw-meta-a-map "p" 'sw-pp)

(define-key sw-meta-a-map "j" 'sw-jmp-to-place)
(define-key sw-meta-a-map "y" 'yank)
(define-key sw-meta-a-map "r" 'insert-register)
(define-key sw-meta-a-map "6" `toggle-buffer-full-filename)

;; https://www.emacswiki.org/emacs/WinnerMode
;; undo/redo window configurations
;; C-c left, C-c right
;; winner-undo, winner-redo
;; will probably bind them to something better
(winner-mode 1)



;; From magit's documentation pages
;; https://magit.vc/manual/magit/Installing-from-an-Elpa-Archive.html#Installing-from-an-Elpa-Archive
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)


;; Inspired by Steve Yegge: let me easily create trash buffers
(defun sw-temp-buffer (buf-name)
  "Switch to a new buffer named buf-name, or to that buffer if it already exists. Is not visiting a file."
  (interactive "sOpen temporary buffer: ")
  (switch-to-buffer buf-name)
  )

(define-key sw-meta-a-map "b" 'sw-temp-buffer)
