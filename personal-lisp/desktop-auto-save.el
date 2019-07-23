;; Copyright 2005 Steve Wainstead (swain@panix.com)

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


;; ABOUT:

;; desktop-auto-save extends the functionality of Emacs's feature of
;; saving your "desktop" (that is, the list of files you have
;; open). It adds the feature of saving the contents of shell buffers
;; to files whenever the desktop is saved.

;; Additionally if you store all the files in a directory and turn
;; that directory into a git repository, desktop-auto-save commits
;; those files when you quit Emacs.

;; put these in your .emacs file or similar.

;; ;; load the desktop on startup (from ~/)
;; (desktop-load-default)
;; ;; automatically save the desktop on exit.
;; (setq desktop-enable t)
;; (load-file "~/.elisp/desktop-auto-save.el")



;; save the desktop every time you auto-save-file
(defun desktop-auto-save ()
  "Added to auto-save-hook so the desktop is not lost."
  (desktop-save "~/") ;; here is why!
  (sw-save-shell-buffer-contents)
  (message "Wrote desktop.")
  )

(add-hook 'auto-save-hook 'desktop-auto-save t)



(defvar sw-restore-shell-buffers-flag t
  "If t, shell buffer contents are inserted from the previous session,
if available.  (This is the default). If nil, previous shell
buffer contents are not restored.  On some systems (or some
versions of systems? Or Emacs?) the shell tries to execute the
contents of the file after insertion, with disasterous
consequences.")


;; For all shell buffers, save their contents to individual files. The
;; idea here is to preserve their contents in case of a crash.
(defun sw-save-shell-buffer-contents ()
  "Find and save all buffers that are in shell-mode"
  (interactive)
  (and (file-accessible-directory-p sw-buffer-file-name-prefix)
       (let ( (bufflist (buffer-list)) ) 
         (while bufflist
           (setq buff (car bufflist))
           (save-excursion
             (set-buffer buff)
             (when (or (string= major-mode "shell-mode") (or (string= (buffer-name) "psql") (string= (buffer-name) "*Python*")) )
               ;;(message (format "it's a shell: %s" (buffer-name buff)))
               ;; if this buffer's contents have not been reloaded...

               ;; thus far, this is just not working out... I want a
               ;; buffer-local variable that indicates whether the buffer
               ;; contents have been inserted. I want to insert them if
               ;; it's nil. What's happening is the variable is "void" and
               ;; desktop-write goes into an infinite loop making system
               ;; beeps.

               ;;(if (and (boundp 'buffer-contents-restored) (not buffer-contents-restored)) (sw-insert-saved-buffer-contents (buffer-name buff)))
               (sw-save-buffer-invisibly buff)
               )
             (setq bufflist (cdr bufflist))
             )
           )
         )
       )
  )

(defvar sw-buffer-file-name-prefix "~/.emacs.shellbuffers/"
  "All shell buffers, when their contents are saved, get this string prefixed to the buffer name.")

;; save a shell buffer's contents to ~/.emacs.shellbuffers/buffer-name; doesn't switch
;; buffer on user; hack from desktop.el which showed how to use write-region
;; to save the contents invisibly
(defun sw-save-buffer-invisibly (buffer)
  "Write out shell buffer's contents for preservation behind-the-scenes."
  (set-buffer buffer)
  (let ( (coding-system-for-write 'no-conversion) ) 
    ;; (write-region (point-min) (point-max) (concat sw-buffer-file-name-prefix (buffer-name buffer) (format-time-string "-%Y-%m-%d"))))
    (write-region (point-min) (point-max) (sw-make-buffer-filename buffer)))
  )

(defun sw-make-buffer-filename (buffer)
  "Make a buffer file name string from the buffer name passed in
by prefixing the string passed in with the contents of the
variable sw-buffer-file-name-prefix. For example: pass in 'cli'
and it returns '~/.emacs.shellbuffers/cli'"
  ;;(message (concat sw-buffer-file-name-prefix buffname))
  (concat sw-buffer-file-name-prefix (buffer-name buffer))
  )

;; so far, no go (no va) FIXME
(defun sw-advance-point-log-buffers ()
  "Saving a window config to register also preserves the location of point, which we 
   don't want. Cycle through the log file buffers and send point to point-max."
  (interactive) 
  (let ( (file-alist sw-tail-file-alist) )
    (while (consp file-alist)
      (setq pair (car file-alist))
      (set-buffer (car pair))
      (message (format "in buffer %s" (car pair)))
      (end-of-buffer)
      (insert "\ntest message\n")
      (setq file-alist (cdr file-alist))
      )
    )
  )


;; The idea here is to "restore" a shell buffer's contents. For
;; example: you have a shell buffer named "foo." The contents of the
;; shell buffer "foo" should be saved when you quit Emacs (saving the
;; contents of all shell buffers is the whole point of this Emacs Lisp
;; file). When you again create a new shell buffer named "foo" the
;; contents of the previous incarnation of "foo" should be loaded into
;; the "foo" shell buffer at the top. Unfortunately it just inserts
;; the text from the previous shell buffer; a future hack will be to
;; insert not the text, but the actual Emacs lisp code (see
;; "buffer-string") representing the shell buffer as it existed
;; previously, including the command history.
(defun sw-insert-saved-buffer-contents (sw-buff-name)
  (message "Inserting auto-desktop-save data...")
  (goto-char (point-min))
  (insert-file (concat sw-buffer-file-name-prefix sw-buff-name))
  (goto-char (point-max))
  ;;(setq buffer-contents-restored t) ;; should be buffer local variable, first created in sw-shell
  )


;; Rather than using version control like git or svn, rename the shell
;; buffer contents file with a date (currently unused)
(defun sw-backup-saved-buffer-contents (sw-buff-name)
  "Make a backup copy of the shell buffer auto save file"
  (message (format "Backing up %s shell buffer contents..." sw-buff-name))
  (rename-file 
   (concat sw-buffer-file-name-prefix sw-buff-name)
   (concat sw-buffer-file-name-prefix sw-buff-name "." (format-time-string "-%Y-%m-%d")))
)


;; deprecated
;; As of april 2009, here's the way to do it; relies on our ability to
;; svn revert the shellbuffer files. You have "within the hour" to
;; restore the shell buffer contents before the cron job commits them
;; to the local svn repository. After that, you'll have to manually
;; retrieve the version you want.
;; (defun sw-restore-shellbuffers-contents ()
;;   "do an svn revert on the shell buffers directory, then for each
;; open buffer, insert its file at point-min"
;;   (interactive)
;;   (shell-command "svn revert ~/.emacs.shellbuffers/*")
;;   ;; now recurse, since they are reverted. Don't want to revert every
;;   ;; time.
;;   (sw-insert-previous-shellbuffer-contents (list "cli" "root" "www" "sql")))

;; deprecated
;; (defun sw-restore-recent-shellbuffers-contents ()
;;   "Don't do an svn revert on the shell buffers directory, then for each
;; open buffer, insert its file at point-min"
;;   (interactive)
;;   (sw-insert-previous-shellbuffer-contents (list "cli" "root" "www" "sql")))

;; recursive function called by sw-restore-shellbuffers-contents
(defun sw-insert-previous-shellbuffer-contents (shell-buffer-list)
  "insert shell buffer contents from a previous session, and
   recurse on the list of shellbuffer names to do the same for each
   shell buffer so named."
  (if (car shell-buffer-list)
      (progn
        (switch-to-buffer (get-buffer (car shell-buffer-list)))
        (goto-char (point-min))
        (insert-file (format "~/.emacs.shellbuffers/%s" (car shell-buffer-list)))
        (goto-char (point-max))
        (sw-insert-previous-shellbuffer-contents (cdr shell-buffer-list))
        )
    )
  )

(defun sw-git-commit-buffers()
  "Save the contents of all shell buffers to their files and then
   git-commit those files in ~/.emacs.shellbuffers."
  (interactive)
  (sw-save-shell-buffer-contents)
  (shell-command "cd ~/.emacs.shellbuffers; git commit -am \"Committing buffers\"")
)
;; when quiting Emacs save and commit the shell buffers
(add-hook 'kill-emacs-hook 'sw-git-commit-buffers t)

