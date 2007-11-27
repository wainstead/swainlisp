;; $Id: mups.el,v 1.6 2007/07/05 19:09:20 swain Exp $

(defvar sw-tail-mups-alist '(
                             ("swg-3" . "swg-3") 
                             ("mup-3" . "mup-3")
                             ("mup-2" . "mup-2")
                             ("mup-1" . "mup-1")
                            )
  "List of mup servers.")


(defun sw-tail-mups ()
  "Tail error logs on the mup servers in shell buffers."
  (interactive)
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((mups-frame (make-frame)))
      (select-frame mups-frame)
      (set-frame-name "mups")))

  (let (pair (file-alist sw-tail-mups-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      (if (not (equal (safe-length file-alist) (safe-length sw-tail-mups-alist)))
          (split-window-vertically))
      (setq pair (car file-alist))
      (shell)
      (rename-buffer (car pair))
      (goto-char (point-max))
      (insert (format "~swain/cvs/mup.exp %s %s" (car pair) (cdr pair)))
      (comint-send-input)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    (balance-windows)
    (window-configuration-to-register ?m))
  )

(defun sw-mup-restart (buffname)
  "Kill and restart a mup buffer."
  (interactive "sMup server: ")
  (kill-buffer (get-buffer buffname))
  (shell)
  (rename-buffer buffname)
  (goto-char (point-max))
  (insert (format "~swain/cvs/mup.exp %s %s" buffname buffname))
  (comint-send-input)
)