(defun sw-pp ()
  "Make iTunes either pause or play"
  (interactive)
  (setq apscript "
tell application \"iTunes\"
	if player state is paused then
		play
	else
		pause
	end if
end tell
"
        )
  (do-applescript apscript)
  )


;; a new command using the do-applescript function
;; TODO: add list of Mac apps that user can interate over via M-p, M-n
(defun sw-switch (app)
  "Switch to app specified by user"
  (interactive "sSwitch to application: ")
  (setq apscript (format "
tell application \"Finder\"
   tell application \"%s\" to activate
end tell
" app))
  (do-applescript apscript))

;; Functions for browsing URL at point have been dropped; use M-x
;; browse-url instead. SW March 2012

;; known bug: Safari won't open this file (the one you are reading
;; right now)
(defun sw-open-buffer-file-in-safari ()
  "Open the file in current buffer in Safari"
  (interactive)
  ;; if buffer is visiting a file:
  (if (buffer-file-name)
      ;; then:
      (progn 
        (setq url (concat "file://" buffer-file-name))
        (setq apscript (format "
tell application \"Safari\"
	activate
        -- either open file in current topmost window or make new window
	if (the (count of windows) is not 0) then
		set the URL of the first document to \"%s\"
	else
		make new document at beginning of documents
		set URL of document 1 to \"%s\"
	end if
end tell
" url url))
        (do-applescript apscript))
    ;; else:
    (message "Can't open this buffer in Safari: buffer is not visiting a file")))

(global-set-key [(control f10)] 'sw-open-buffer-file-in-safari)

