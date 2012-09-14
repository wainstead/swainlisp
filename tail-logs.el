;; The "meta" functions came later... see comment staring with "This
;; first block..."

(defun sw-tail-logs-meta (ssc-alist ssc-frame-name)
  "meta function for opening logs and tailing them in a new frame"
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((logs-frame (make-frame)))
      (select-frame logs-frame)
      (set-frame-width (selected-frame) 250)
      (set-frame-height (selected-frame) 84)
      (set-frame-name ssc-frame-name)))

  (let (pair (file-alist ssc-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      (if (not (equal (safe-length file-alist) (safe-length ssc-alist)))
          (split-window-vertically))
      (setq pair (car file-alist))
      (shell)
      (rename-buffer (car pair))
      (goto-char (point-max))
      (if (cdr pair)
          ;; 'nil' indicates "don't tail any log"
          ;; Future project: pass in a function to do anything
          (insert (format "~/bin/waittail %s" (cdr pair)))
      )
      (comint-send-input)
      (balance-windows)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    (balance-windows)
    (window-configuration-to-register ?1))
  )
 
;; undo the work of sw-tail-logs-meta
(defun sw-kill-logs-meta (ssc-alist ssc-frame-name)
  "Kill the buffers tailing the log files as listed in ssc-alist."
  (if (y-or-n-p "Really kill the buffers that are tailing the log files? ")
      (progn
        ;; FIXME: Should fail gracefully if the buffer doesn't exist anymore
        (switch-to-buffer (car (car ssc-alist)))
        (delete-other-windows)
        (let ((file-alist ssc-alist))
          (while (consp file-alist)
            (setq pair (car file-alist))
            (unless (kill-buffer (car pair))
              (message (format "Couldn't kill the buffer %s." (car pair))))
            (setq file-alist (cdr file-alist))
            ))
        (when window-system
          (select-frame-by-name ssc-frame-name)
          (delete-frame))
        )
    ;; else:
        (message "Log tailing buffers not deleted.")))

(defun sw-fix-logs ()
  "Colorize the window that tails logs."
  (interactive)
  ;;(set-default-font "-adobe-courier-medium-r-normal-*-*-120-*-*-*-*-iso8859-1")
  (set-background-color "#202020")
  (set-foreground-color "goldenrod")
)


;; This first block comes originally from swainlib.el, and is how I
;; started down the road of tailing log files in Emacs. Later I wrote
;; sw-tail-logs-meta to accomodate opening multiple frames with
;; different sets of logs being tailed; this I did for MyPhotoAlbum.

;; use defun globally; setq this in the local .emacs file in your home dir
(defvar sw-tail-file-alist '(
                             ("apache access log" . "/usr/local/apache/logs/access_log")
                             ("apache error log" . "/usr/local/apache/logs/error_log")
                             )
  "List of log files with names for buffers. Used by sw-tail-logs and sw-kill-logs.")


(defun sw-tail-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-file-alist."
  (interactive)
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
      (let ((logs-frame (make-frame)))
        (select-frame logs-frame)
        (set-frame-name "logs")
        (if (functionp 'sw-fix-logs)
            (progn
              (sw-fix-logs)
              (sw-colors "303030")
              (set-frame-width (selected-frame) 250)
              (set-frame-height (selected-frame) 84)
              (enlarge-window -25)
              )
          )
        )
    )
    
  (let (pair (file-alist sw-tail-file-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      (if (not (equal (safe-length file-alist) (safe-length sw-tail-file-alist)))
          (split-window-vertically))
      (setq pair (car file-alist))
      (shell)
      (rename-buffer (car pair))
      (goto-char (point-max))
      (insert (format "tail -f `ls -t %s* | head -1`" (cdr pair)))
      (comint-send-input)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    ;(balance-windows)
    (window-configuration-to-register ?1))
  )
 
;; undo the work of sw-tail-logs
(defun sw-kill-logs ()
  "Kill the buffers tailing the log files as listed in sw-tail-file-alist."
  (interactive)
  (if (y-or-n-p "Really kill the buffers that are tailing the log files? ")
      (progn
        (switch-to-buffer (car (car sw-tail-file-alist)))
        (delete-other-windows)
        (let ((file-alist sw-tail-file-alist))
          (while (consp file-alist)
            (setq pair (car file-alist))
            (unless (kill-buffer (car pair))
              (message (format "Couldn't kill the buffer %s." (car pair))))
            (setq file-alist (cdr file-alist))
            ))
        (when window-system
          (select-frame-by-name "logs")
          (delete-frame))
        )
    ;; else:
    (message "Log tailing buffers not deleted.")))


