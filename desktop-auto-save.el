;; Copyright 2005 Steve Wainstead (swain@panix.com)

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; put these in your .emacs file or similar.

;; ;; load the desktop on startup (from ~/)
;; (desktop-load-default)
;; ;; automatically save the desktop on exit.
;; (setq desktop-enable t)
;; (load-file "~swain/.elisp/desktop-auto-save.el")



;; save the desktop every time you auto-save-file
(defun desktop-auto-save ()
  "Added to auto-save-hook so the desktop is not lost."
  (desktop-save "~/")
  (sw-save-shell-buffer-contents)
  (message "Wrote desktop.")
  )

(add-hook 'auto-save-hook 'desktop-auto-save t)



(defvar sw-restore-shell-buffers-flag t
  "If t, shell buffer contents are inserted from the previous session.
if available.  (This is the default). If nil, previous shell buffer
contents are not restored.  This is necessary on some systems where
the shell tries to execute the contents of the file.")


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
             (when (string= major-mode "shell-mode")
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

(defvar sw-buffer-file-name-prefix "~swain/.emacs.shellbuffers/"
  "All shell buffers, when their contents are saved, get this string prefixed to the buffer name.")

;; save a shell buffer's contents to ~/.emacs.shellbuffers/buffer-name; doesn't switch
;; buffer on user; hack from desktop.el which showed how to use write-region
;; to save the contents invisibly
(defun sw-save-buffer-invisibly (buffer)
  "Write out shell buffer's contents for preservation behind-the-scenes."
  (set-buffer buffer)
  (let ( (coding-system-for-write 'no-conversion) ) 
    ;; (write-region (point-min) (point-max) (concat sw-buffer-file-name-prefix (buffer-name buffer) (format-time-string "-%Y-%m-%d"))))
    (write-region (point-min) (point-max) (sw-saved-buffer-filename (buffer-name buffer))))
  )

(defun sw-saved-buffer-filename (buffname)
  "Make a buffer file name from the buffer name passed in."
  "For example: pass in 'cli' and it returns '~swain/.emacs.shellbuffers/cli'"
  (interactive "sname: ")
  (message (concat sw-buffer-file-name-prefix buffname))
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


(defun sw-insert-saved-buffer-contents (sw-buff-name)
  (message "Inserting auto-desktop-save data...")
  (goto-char (point-min))
  (insert-file (concat sw-buffer-file-name-prefix sw-buff-name))
  (goto-char (point-max))
  ;;(setq buffer-contents-restored t) ;; should be buffer local variable, first created in sw-shell
  )


(defun sw-backup-saved-buffer-contents (sw-buff-name)
  "Make a backup copy of the shell buffer auto save file"
  (message (format "Backing up %s shell buffer contents..." sw-buff-name))
  (rename-file 
   (concat sw-buffer-file-name-prefix sw-buff-name)
   (concat sw-buffer-file-name-prefix sw-buff-name "." (format-time-string "-%Y-%m-%d")))
)

(defun sw-forcibly-insert-buffer ()
  "yay"
  (interactive)
  (sw-insert-saved-buffer-contents (buffer-name))
)

;; as of april 2009, here's the way to do it; relies on our ability to
;; svn revert the shellbuffer files. You have "within the hour" to
;; restore the shell buffer contents before the cron job commits them
;; to the local svn repository. After that, you'll have to manually
;; retrieve the version you want.
(defun sw-restore-shellbuffers-contents ()
  "do an svn revert on the shell buffers directory, then for each
open buffer, insert its file at point-min"
  (interactive)
  (shell-command "svn revert ~swain/.emacs.shellbuffers/*")
  ;; now recurse, since they are reverted. Don't want to revert every
  ;; time.
  (sw-insert-previous-shellbuffer-contents (list "cli" "root" "www" "sql")))

(defun sw-restore-recent-shellbuffers-contents ()
  "don't do an svn revert on the shell buffers directory, then for each
open buffer, insert its file at point-min"
  (interactive)
  (sw-insert-previous-shellbuffer-contents (list "cli" "root" "www" "sql")))

;; recursive function called by sw-restore-shellbuffers-contents
(defun sw-insert-previous-shellbuffer-contents (shell-buffer-list)
  "insert shell buffer contents from a previous session, and
   recurse on the list of shellbuffer names to do the same for each
   shell buffer so named."
  (if (car shell-buffer-list)
      (progn
        (switch-to-buffer (get-buffer (car shell-buffer-list)))
        (goto-char (point-min))
        (insert-file (format "~swain/.emacs.shellbuffers/%s" (car shell-buffer-list)))
        (goto-char (point-max))
        (sw-insert-previous-shellbuffer-contents (cdr shell-buffer-list))
        )
    )
  )

;; save *compilation* buffer to file
;; git commit it right after

(defun sw-write-compilation-buffer()
  "Hook function to write the *compilation* buffer for each compile."
  (and (get-buffer "*compilation*") (sw-save-buffer-invisibly (get-buffer "*compilation*")))
  (shell-command "cd ~swain/.emacs.shellbuffers; git commit -m \"saving last compilation\" ?compilation?")
)

;; every time the "compile" or "recompile" functions are run, write
;; out the *compilation* buffer's contents and commit it.
(defadvice compile (before sw-save-last-compilation activate compile)
  "Every time we compile, save the previous compilation to the
   ~/.emacs.shellbuffers directory and git commit it."
  (sw-write-compilation-buffer))

(defadvice recompile (before sw-save-last-compilation activate compile)
  "Every time we compile, save the previous compilation to the
   ~/.emacs.shellbuffers directory and git commit it."
  (sw-write-compilation-buffer))

;; This turned out to be a not-so-good idea because erase-buffer is
;; used by a variety of other functions. Otherwise it was a stellar
;; idea.
;; (defadvice erase-buffer (before sw-git-commit-buffers activate compile)
;;   "Whenever I hit F3 to clear a buffer, go into the
;;    ~/.emacs.shellbuffers directory and git commit all the buffers."
;;   (sw-git-commit-buffers)
;;   )

(defun sw-git-commit-buffers()
  "Save the contents of all shell buffers to their files and then
   git-commit those files in ~/.emacs.shellbuffers."
  (sw-save-shell-buffer-contents)
  (shell-command "cd ~swain/.emacs.shellbuffers; git commit -am \"Committing buffers\"")
)
;; when qutting Emacs save and commit the shell buffers
(add-hook 'kill-emacs-hook 'sw-git-commit-buffers t)
