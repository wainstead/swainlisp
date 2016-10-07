;; Copyright 2012 Steve Wainstead (swain@panix.com)

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
  ;; if we are in the frame holding the compilation buffer, don't switch frames.
  (and (not(string= (buffer-name) "*compilation*")) (switch-to-buffer-other-frame "*compilation*"))
  (sw-write-compilation-buffer)
  ;;(sw-randomize-frame-colors)
)


(defadvice recompile (before sw-save-last-compilation activate compile)
  "Every time we compile, save the previous compilation to the
   ~/.emacs.shellbuffers directory and git commit it."
  (and (not(string= (buffer-name) "*compilation*")) (switch-to-buffer-other-frame "*compilation*"))
  (sw-write-compilation-buffer)
  ;;(sw-randomize-frame-colors)
)

;; This turned out to be a not-so-good idea because erase-buffer is
;; used by a variety of other functions. Otherwise it was a stellar
;; idea.
;; (defadvice erase-buffer (before sw-git-commit-buffers activate compile)
;;   "Whenever I hit F3 to clear a buffer, go into the
;;    ~/.emacs.shellbuffers directory and git commit all the buffers."
;;   (sw-git-commit-buffers)
;;   )


;; choose random colors every time we compile, just for fun
(defun sw-make-random-hex-color-string ()
  "Return a string in the form of #FFFFFF. Choose the number for
   #xffffff randomly using Emacs Lisp's builtin function (random)."
  ;; seed our random number generator: current datetime plus Emacs's
  ;; process ID
  (random t)
  (format "#%06x" (random #xffffff))
  )

;; via stackoverflow: formatting date/time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
  See help of `format-time-string' for possible replacements")

(defun sw-randomize-frame-colors ()
  "Change foreground and background colors of the current frame to
random colors."
  (interactive)
  (let 
      (
       (fg-color (sw-make-random-hex-color-string)) 
       (bg-color (sw-make-random-hex-color-string))
       (color-distance #x3fffff)
       )
    ;; Might crash Emacs. See:
    ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-06/msg00869.html
    (if (not (eq fg-color bg-color))
        (progn
          (shell-command (format "/bin/echo '%s nb %s nf %s' >> /Users/swain/.emacs.shellbuffers/colorchanges\n" 
                                 (format-time-string current-date-time-format (current-time))
                                 bg-color
                                 fg-color))
          (set-background-color bg-color)
          (shell-command (format "/bin/echo 'background set' >> /Users/swain/.emacs.shellbuffers/colorchanges\n"))
          (set-foreground-color fg-color)
          (shell-command (format "/bin/echo 'foreground set' >> /Users/swain/.emacs.shellbuffers/colorchanges\n"))
          )
      ;; else
      (message "Not changing colors, new foreground and background colors were identical"))
    )
  )

