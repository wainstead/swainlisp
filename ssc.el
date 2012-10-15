;; The following copied from:
;; http://orgmode.org/manual/Activation.html#Activation
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; recommended global key bindings for org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; add timestamp to DONE items in a todo list
(setq org-log-done 'time)

;; save commonly needed strings in registers
(set-register ?s "\\i /Users/swain/.psql_connect")
(set-register ?d "import datasource; con = datasource.quick_setup(site='nfmc')")
(set-register ?i "cd ~/git/pippin/sites/nfmc-reporting/mods; ipython2.4  -i model.py")
(set-register ?l "begin; select nfmc.set_current_user_and_ip(1251, '127.0.0.1'); commit;")
(set-register ?c "^\\s-*class \\|^\\s-*def ")
(set-register ?p "SELECT * FROM pg_stat_activity;")

(global-set-key [(meta ?)] 'other-window)

;; set up stuff for sql-postgres
(setq sql-postgres-program "/usr/local/pgsql/bin/psql")
(setq sql-user "social")
(setq sql-database "nworks")
(setq sql-server "localhost")

;;(load-file "~swain/.elisp/ipython.el")

(defun insert-tabs-hook-func ()
  "Set up indentation for SSC, which prefers tabs for indentation."
  (setq indent-tabs-mode t)
  (setq tab-width 4)
)

;; .html and .template files are usually Cheetah template files
(add-to-list 'auto-mode-alist '("\\.html$"   . cheetah-mode       ))
(add-to-list 'auto-mode-alist '("\\.tmpl$"   . cheetah-mode       ))
(add-to-list 'auto-mode-alist '("\\.html.template$"   . cheetah-mode       ))
(add-to-list 'auto-mode-alist '("\\.html.translate$"   . cheetah-mode       ))

(add-hook 'python-mode-hook 'insert-tabs-hook-func)
(add-hook 'sql-mode-hook 'insert-tabs-hook-func)
(add-hook 'cheetah-mode-hook 'insert-tabs-hook-func)

;; don't add trailing whitespace
(add-hook 'python-mode-hook
		  (lambda ()
			(setq show-trailing-whitespace t)))

(setq compile-command "cd ~swain/git/pippin; make nfmc")

(blink-cursor-mode 0)
(transient-mark-mode t)
(set-mouse-color "white") ;; what does this do? Probably an X11 thing.

(put 'narrow-to-region 'disabled nil)
(setq mac-command-modifier 'meta)

(defun sw-pippin ()
  "jump to the top"
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin")
  (comint-send-input))

(defun sw-postgresql ()
  "Move the cli shell into the postres directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/postgres/nfmc/")
  (comint-send-input))

(defun sw-mods ()
  "Move the cli shell into the nfmc mods directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/sites/nfmc-reporting/mods/")
  (comint-send-input))

(defun sw-mods/nfmcreporting ()
  "Move the cli shell into the mods/nfmcreporting directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/sites/nfmc-reporting/mods/nfmcreporting/")
  (comint-send-input))

(defun sw-mods/tests ()
  "Move the cli shell into the mods/tests directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/sites/nfmc-reporting/mods/tests/")
  (comint-send-input))

;; kinda dumb but I like that I can use periods in function names
(defun sw-mods/.. ()
  "Move the cli shell into the mods/.. directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/sites/nfmc-reporting/")
  (comint-send-input))

(defun sw-templates ()
  "Move the cli shell into the nfmc templates directory."
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin/sites/nfmc-reporting/templates/")
  (comint-send-input))


(defun sw-repl ()
  "Open a py-shell buffer, rename it 'repl' for ipython"
  (interactive)
  (py-shell)
  (rename-buffer "repl")
  )

(defalias 'sw-console 'sw-repl)

(defun sw-psql ()
  "Open a SQLi buffer, rename it 'psql'"
  (interactive)
  (sql-postgres)
  (rename-buffer  "psql")
  (goto-char (point-max))
  )

;; I should write something that takes an alist of shell name:
;; starting directory, and iterates over the alist creating shell
;; buffers. It would reduce redundant code. This alist would be
;; project-specific; if it's SSC, for example, it's all the logs I
;; want for that versus Waverous.
(defun sw-nose ()
  "Open a new bash shell, put it in the ~/git/pippin dir for nose tests."
  (interactive)
  (sw-shell "nosetests")
  (insert "cd ~/git/pippin")
  (comint-send-input)
)

;; store the string for setting the search path in psql in register 's'
(set-register ?s "set search_path=nfmc,public;")

;; not running start.dev in Emacs anymore... 
;; (defun sw-start.dev ()
;;   "Open a shell buffer, rename it 'start.dev'"
;;   (interactive)
;;   (sw-shell "start.dev")
;;   (goto-char (point-max))
;;   )

;; Though I have no current use for it, Dale found a Javascript
;; interpreter that comes with OS X.
(defun sw-js-repl ()
  "Open the OS X JavaScript repl tool"
  (interactive)
  (sw-shell "javascript repl")
  (goto-char (point-max))
  (insert "/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc")
  (comint-send-input)
)

;; found this on the net somewhere
(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\|echo\\|return\\|silent\\|elif\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  )

(defvar sw-tail-nfmc-frame-name "nfmc logs" "Frame name for the nfmc logs")
(defvar sw-tail-nfmc-alist '(
                             ("pippin log"      . "/tmp/pippin.log")
                             ("elsewhere log"   . "/tmp/nfmc-laborer.log")
                             ("swallower log"   . "/tmp/nfmc-csv-swallower.log")
                             ("error log"       . "/opt/local/apache2/logs/error_log | egrep -v '^Normal|^Finished'")
                             ("nfmc error log"  . "/opt/local/apache2/logs/nfmc-reporting_error_log | egrep -v '^Normal|^Finished'")
                             ("nfmc access log" . "/opt/local/apache2/logs/nfmc-reporting_access_log")
                             )
  "List of nfmc log files with names for buffers. Used by sw-tail-nfmc-logs and sw-kill-nfmc-logs.")

(defun sw-tail-nfmc-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-nfmc-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-nfmc-alist sw-tail-nfmc-frame-name)
  (progn
    (select-frame-by-name sw-tail-nfmc-frame-name)
    (sw-fix-logs)
    (sw-colors "003030")
    (enlarge-window -25)
    (window-configuration-to-register ?2)
    )
  )

(defun sw-kill-nfmc-logs ()
  (interactive)
  (sw-kill-logs-meta sw-tail-nfmc-alist sw-tail-nfmc-frame-name))


;; todo: after running mergeancestors, or similar, reload all source
;; files and set the point to where it was.

;; for each python or cheetah or sql buffer:
;; save-excursion
;; load-file (current-file)
;; maybe that's all?


(defun sw-reload-files ()
  "Reload all Python, Cheetah and SQL files; needed after a git
script has run. Otherwise Emacs keeps asking if I really want to
edit the file because it changed on disk."
  (interactive)
  (let ( (bufflist (buffer-list)) ) 
    (while bufflist
      (setq buff (car bufflist))
      (save-excursion
        (set-buffer buff)
        (when (or (string= major-mode "sql-mode")
                  (string= major-mode "cheetah-mode")
                  (string= major-mode "python-mode"))
          (message (format "found a %s buffer" major-mode))
          (find-file (buffer-file-name))

          )
        (setq bufflist (cdr bufflist)) ; this probably should be done recursively
        )
      )
    )
  )


(defun sw-lint ()
  "Run a lint check on the file the current buffer is
   visiting. Thanks to Dale for the python incantation."
  (interactive)
  (if buffer-file-name
      (let ( (pylint-interpreter "/usr/bin/env python -m py_compile") ) 
        (shell-command (format "%s %s" pylint-interpreter (buffer-file-name))) 
        )
    (message "Buffer not visiting a file.")
    )
  )
(global-set-key [(f4)] 'sw-lint)


;; One function and two convenience commands for running git diff and
;; putting the results in a special window.
(defun sw-git-diff-meta (output-buffer-name git-command)
  "Run git-command as a shell command; output to
   output-buffer-name."
  (switch-to-buffer (get-buffer-create output-buffer-name))
  (diff-mode)
  (shell-command git-command output-buffer-name)
  (hi-lock-unface-buffer "^diff.*")
  (hi-lock-face-buffer "^diff.*" "hi-yellow")
  (toggle-read-only)
)
(defun sw-git-diff ()
  "Run git diff, output to new buffer"
  (interactive) 
  (sw-git-diff-meta "*git diff*" "cd ~swain/git/pippin; git diff")
  )

(defun sw-git-diff-master ()
  "Run git diff master HEAD, output to new buffer"
  (interactive)
  (sw-git-diff-meta "*git diff master*" "cd ~swain/git/pippin; git diff origin/master HEAD")
  )


;; copy screencaps to the directory hardwired into
;; ~swain/bin/mv-screencap.sh
(defun sw-screencap (new-filename)
  "Move/rename a screen capture from $HOME/Desktop, taken with
   Command-Shift-4 or similar."
  (interactive "sName screen capture: ")
  (shell-command (format "~swain/bin/mv-screencap.sh %s" new-filename))
  (insert (format "screencaps/%s.png" new-filename))
  )


;; save point, reload file, jump to point
(fset 'sw-reload-file
   [?\C-x ?r ?  ?n ?\C-x ?\C-v return ?\C-x ?r ?j ?n])
(global-set-key [(f13)] 'sw-reload-file)
(global-set-key [(f15)] 'sw-randomize-frame-colors)

