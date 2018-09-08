(setq custom-file "~/.elisp/emacs-harmonika-custom.el")
(load custom-file)
(tool-bar-mode -1)

;;(setq mac-allow-anti-aliasing nil)

;; this is the local file.
(when window-system
  ;; make pretty
  (setq default-frame-alist '((background-color . "#040000")
                              (foreground-color . "goldenrod")
                              (cursor-color     . "green"))))


;; for all places:
(load-file "~/.elisp/swainlib.el")
;(load-file "~/.elisp/mups.el")
;(load-file "~/.elisp/mpp.el")

;; for cocoa programming
;;(setq compile-command "xcodebuild")

(blink-cursor-mode 0)
(transient-mark-mode t)
(set-mouse-color "white")

(defun sw-projects ()
  "we need more zippy pinheadisms"
  (interactive)
  (sw-shell "cli")
  (goto-char (point-max))
  (insert "cd ~swain/Sites/projects/")
  (comint-send-input)
  )

(defun sw-moo ()
  "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
  (interactive)
  (sw-shell "cli")
  (sw-goto-moo)
  (sw-shell "server")
  (sw-goto-moo)
  )

(defun sw-goto-moo ()
  (goto-char (point-max))
  (insert "cd ~swain/Sites/projects/MOO/")
  (comint-send-input)
  )


(defun sw-waverous ()
  "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
  (interactive)
  (goto-char (point-max))
  (insert "cd ~swain/Sites/projects/moowork/waverous/server/")
  (comint-send-input)
  )

(defun sw-tunestead ()
  "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
  (interactive)
  (goto-char (point-max))
  (insert "cd ~swain//Users/swain/Sites/projects/rails/tunestead/")
  (comint-send-input)
  )

;;(require 'rmoo-autoload)


(put 'narrow-to-region 'disabled nil)
(transient-mark-mode t)


;; the big enchilada
(defun sw-rails ()
  "Are we live or on tape?"
  (interactive)

  (sw-shell "console")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/rails/tunestead")
  (comint-send-input)

  (sw-shell "mongrel")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/rails/tunestead")
  (comint-send-input)


  (sw-shell "cli")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/rails/tunestead")
  (comint-send-input)

  (sw-shell "tail")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/rails/tunestead")
  (comint-send-input)

)

(setq mac-command-modifier 'meta)

(sw-shell "cli")
(global-set-key "\M-`" 'other-frame)
(global-set-key [(meta ?)] 'other-window)

;; Gonna try living with an iBuffer that no longer removes all other
;; windows.
(defun sw-list ()
  "switch to Ibuffer; this function should be bound to F9"
  (interactive)
  (setq buffer (get-buffer "*Ibuffer*"))
  (if (bufferp buffer)
      (progn 
        (switch-to-buffer buffer)
        (ibuffer-update nil))
    (ibuffer))
  ;;(delete-other-windows)
  )
(put 'set-goal-column 'disabled nil)


;; set up stuff for sql-mysql
;; (setq sql-mysql-program "/Applications/XAMPP/xamppfiles/bin/mysql")
;; (setq sql-user "root")
;; (setq sql-database "bandspace")
;; (setq sql-server "localhost")
(put 'upcase-region 'disabled nil)

;; For SSC, I like compilation in a separate frame. Not so much for
;; Waverous.
(ad-unadvise 'compile)
(ad-unadvise 'recompile)
