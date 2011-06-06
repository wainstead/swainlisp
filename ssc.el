(defun insert-tabs-hook-func ()
  "Set up indentation for SSC, which prefers tabs for indentation."
  (setq indent-tabs-mode t)
  (setq tab-width 4)
)
(add-hook 'python-mode-hook 'insert-tabs-hook-func)
(add-hook 'sql-mode-hook 'insert-tabs-hook-func)


(setq compile-command "cd ~swain/git/pippin; make nfmc")

(blink-cursor-mode 0)
(transient-mark-mode t)
(set-mouse-color "white")

(put 'narrow-to-region 'disabled nil)
(setq mac-command-modifier 'meta)

(defun sw-pippin ()
  "jump to the top"
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin")
  (comint-send-input))

(defun sw-psql ()
  "Open a shell buffer, rename it 'psql'"
  (interactive)
  (sw-shell "psql")
  (goto-char (point-max))
  (insert "set search_path=nfmc,public;")
  )

(defun nfmc-search-path ()
  "insert the search path string"
  (interactive)
  (insert "set search_path=nfmc,public;")
)

(defun sw-start.dev ()
  "Open a shell buffer, rename it 'start.dev'"
  (interactive)
  (sw-shell "start.dev")
  (goto-char (point-max))
  )


(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  )



(defvar sw-tail-nfmc-frame-name "nfmc logs" "Frame name for the nfmc logs")
(defvar sw-tail-nfmc-alist '(
                             ("pippin log" . "/tmp/pippin.log")
                             ;;("nfmc func log" . "/tmp/osc_func.log")
                             ("error log" . "/opt/local/apache2/logs/error_log")
                             )
  "List of nfmc log files with names for buffers. Used by sw-tail-nfmc-logs and sw-kill-nfmc-logs.")

(defun sw-tail-nfmc-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-nfmc-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-nfmc-alist sw-tail-nfmc-frame-name)
  (progn
    (select-frame-by-name sw-tail-nfmc-frame-name)
    (sw-fix-logs)
    (sw-colors "003030")
    (set-frame-width (selected-frame) 250)
    (set-frame-height (selected-frame) 84)
    (enlarge-window -25)
    (window-configuration-to-register ?2)
    )
  )

(defun sw-kill-nfmc-logs ()
  (interactive)
  (sw-kill-logs-meta sw-tail-nfmc-alist sw-tail-nfmc-frame-name))



(defun sw-tail-logs-meta (store-alist store-frame-name)
  "meta function for opening logs and tailing them in a new frame"
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((logs-frame (make-frame)))
      (select-frame logs-frame)
      (set-frame-name store-frame-name)))

  (let (pair (file-alist store-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      (if (not (equal (safe-length file-alist) (safe-length store-alist)))
          (split-window-vertically))
      (setq pair (car file-alist))
      (shell)
      (rename-buffer (car pair))
      (goto-char (point-max))
      (insert (format "tail -f %s" (cdr pair)))
      (comint-send-input)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    (balance-windows)
    (window-configuration-to-register ?1))
  )
 
;; undo the work of sw-tail-logs-meta
(defun sw-kill-logs-meta (store-alist store-frame-name)
  "Kill the buffers tailing the log files as listed in store-alist."
  (if (y-or-n-p "Really kill the buffers that are tailing the log files? ")
      (progn
        (switch-to-buffer (car (car store-alist)))
        (delete-other-windows)
        (let ((file-alist store-alist))
          (while (consp file-alist)
            (setq pair (car file-alist))
            (unless (kill-buffer (car pair))
              (message (format "Couldn't kill the buffer %s." (car pair))))
            (setq file-alist (cdr file-alist))
            ))
        (when window-system
          (select-frame-by-name store-frame-name)
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


