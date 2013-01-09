;; This largely now works, but the formatting of the new frame is all
;; whack. There might be some disadvantage to using
;; async-shell-command instead of bash, ultimately; like sending
;; control-c and then restarting tailing the log file... but perhaps
;; there is a function in the ^comint- family that can replace the
;; current process, say something that uses 'exec'.

(defun sw-kill-async-comint-buffer (buffname)
  "Kill a buffer that's probably tailing a log"
  (interactive "sBuffer name: ")
  (switch-to-buffer buffname)
  (comint-kill-subjob)
  ;;(comint-send-eof)
  (while (comint-check-proc (get-buffer buffname)))
  (message "Done waiting for buffer to die")
  (kill-buffer buffname)
)


;; rethinking the problem: don't invoke bash at all.
(defun sw-tail-logs-meta (commands-alist tail-frame-name)
  "meta function for opening logs and tailing them in a new frame"
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((logs-frame (make-frame)))
      (select-frame logs-frame)
      (set-frame-width (selected-frame) 250)
      (set-frame-height (selected-frame) 84)
      (set-frame-name tail-frame-name)))

  (let (pair (file-alist commands-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      ;; (if (not (equal (safe-length file-alist) (safe-length commands-alist)))
      ;;     (split-window-vertically))
      (setq pair (car file-alist))
      (async-shell-command (format "~/bin/waittail %s" (cdr pair)) (car pair))
      ;;(message (format "(async-shell-command \"~/bin/waittail %s\"  \"%s\")" (cdr pair) (car pair)))
      (split-window-vertically)
      (balance-windows)
      ;;(rename-buffer (car pair))
      ;;(goto-char (point-max))
      ;; (if (cdr pair)
      ;;     ;; 'nil' indicates "don't tail any log"
      ;;     ;; Future project: pass in a function to do anything
      ;;     (insert (format "~/bin/waittail %s" (cdr pair)))
      ;; )
      ;; (comint-send-input)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    (balance-windows)
    (window-configuration-to-register ?1))
  )

;; undo the work of sw-tail-logs-meta
(defun sw-kill-logs-meta (commands-alist tail-frame-name)
  "Kill the buffers tailing the log files as listed in commands-alist."
  (if (y-or-n-p "Really kill the buffers that are tailing the log files? ")
      (progn
        ;; FIXME: Should fail gracefully if the buffer doesn't exist anymore
        ;; FIXME: probably should use recursion instead of a 'while' loop
        (switch-to-buffer (car (car commands-alist)))
        (delete-other-windows)
        (let ((file-alist commands-alist))
          (while (consp file-alist)
            (setq pair (car file-alist))
            (unless (sw-kill-async-comint-buffer (car pair))
              (message (format "Couldn't kill the buffer %s." (car pair))))
            (setq file-alist (cdr file-alist))
            ))
        (when window-system
          (select-frame-by-name tail-frame-name)
          (delete-frame))
        )
    ;; else:
        (message "Log tailing buffers not deleted."))
  )


; (defun sw-kill-comint-buffer ()
;   "Kill a buffer that's probably tailing a log"
;   (interactive)
;   (switch-to-buffer "tail")
;   (comint-kill-subjob)
;   (comint-send-eof)
;   (while (comint-check-proc (get-buffer "tail")))
;   (message "Done waiting for buffer to die")
;   (kill-buffer "tail")
; )

(defun sw-frame-only ()
  (interactive)
  (sw-just-make-a-frame sw-tail-nfmc-alist sw-tail-nfmc-frame-name)
  (progn
    (select-frame-by-name sw-tail-nfmc-frame-name)
    (sw-color-logs)
    (sw-colors "003030")
    )
  )

(sw-frame-only)
(and (async-shell-command "~/bin/waittail /tmp/pippin.log"  "pippin log") (split-window-vertically))
(async-shell-command "~/bin/waittail /tmp/nfmc-laborer.log"  "elsewhere log")
(async-shell-command "~/bin/waittail /tmp/nfmc-csv-swallower.log"  "swallower log")
(async-shell-command "~/bin/waittail /opt/local/apache2/logs/error_log | egrep -v '^Normal|^Finished'"  "error log")
(async-shell-command "~/bin/waittail /opt/local/apache2/logs/nfmc-reporting_error_log | egrep -v '^Normal|^Finished'"  "nfmc error log")
(async-shell-command "~/bin/waittail /opt/local/apache2/logs/nfmc-reporting_access_log"  "nfmc access log")

;; the problem with this approach is that the buffer is in fundamental mode
(and (start-process "pippin log" "pippin log" "waittail" "/tmp/pippin.log") (switch-to-buffer "pippin log"))

(defun sw-just-make-a-frame (commands-alist tail-frame-name)
  "meta function for opening logs and tailing them in a new frame"
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((logs-frame (make-frame)))
      (select-frame logs-frame)
      (set-frame-width (selected-frame) 250)
      (set-frame-height (selected-frame) 84)
      (set-frame-name tail-frame-name)))
  )

(defun sw-tail-nfmc-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-nfmc-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-nfmc-alist sw-tail-nfmc-frame-name)
  (progn
    (select-frame-by-name sw-tail-nfmc-frame-name)
    (sw-color-logs)
    (sw-colors "003030")
    (enlarge-window -25)
    (window-configuration-to-register ?2)
    )
  )
