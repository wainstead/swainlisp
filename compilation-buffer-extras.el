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
  (sw-randomize-frame-colors)
)


(defadvice recompile (before sw-save-last-compilation activate compile)
  "Every time we compile, save the previous compilation to the
   ~/.emacs.shellbuffers directory and git commit it."
  (and (not(string= (buffer-name) "*compilation*")) (switch-to-buffer-other-frame "*compilation*"))
  (sw-write-compilation-buffer)
  (sw-randomize-frame-colors)
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
    (set-foreground-color fg-color)
    (set-background-color bg-color)
    )
  )
