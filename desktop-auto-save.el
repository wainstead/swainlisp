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
