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

;; $Id: desktop-auto-save.el,v 1.2 2006/02/22 19:41:22 swain Exp $


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



;; For all shell buffers, save their contents to individual files. The
;; idea here is to preserve their contents in case of a crash.
(defun sw-save-shell-buffer-contents ()
  "Find and save all buffers that are in shell-mode"
  (interactive) 
  (let ( (bufflist (buffer-list)) ) 
    (while bufflist
      (setq buff (car bufflist))
      (save-excursion
        (set-buffer buff)
        (when (string= major-mode "shell-mode")
          ;;(message (format "it's a shell: %s" (buffer-name buff)))
          (sw-save-buffer-invisibly buff)
          )
        (setq bufflist (cdr bufflist))
        )
      )
    )
  )

(defvar sw-buffer-file-name-prefix "~swain/.emacs.shellbuffer."
  "All shell buffers, when their contents are saved, get this string prefixed to the buffer name.")

;; save a shell buffer's contents to ~/.emacs.shellbuffer.buffer-name; doesn't switch
;; buffer on user; hack from desktop.el which showed how to use write-region
;; to save the contents invisibly
(defun sw-save-buffer-invisibly (buffer)
  "Write out shell buffer's contents for preservation behind-the-scenes."
  (set-buffer buffer)
  (let ( (coding-system-for-write 'no-conversion) ) 
    (write-region (point-min) (point-max) (concat sw-buffer-file-name-prefix (buffer-name buffer))))
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

