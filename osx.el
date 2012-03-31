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


;; use AppleScript to tell Mozilla to open URL at point
;; some code borrowed from browse-url-mozilla
(defun sw-mozilla-browse-url (url &optional new-window)
  "Open the url at or around point"
  (interactive (browse-url-interactive-arg "URL: "))
  (while (string-match "[,)]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (setq apscript (format "
tell application \"Finder\"
   tell application \"Mozilla\"
      activate
      Get URL \"%s\"
   end tell
end tell
" url))
  (do-applescript apscript))

;; use AppleScript to tell Firefox to open URL at point
;; some code borrowed from browse-url-mozilla
(defun sw-firefox-browse-url (url &optional new-window)
  "Open the url at or around point"
  (interactive (browse-url-interactive-arg "URL: "))
  (while (string-match "[,)]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (setq apscript (format "
tell application \"Finder\"
   tell application \"Firefox\"
      activate
      Get URL \"%s\"
   end tell
end tell
" url))
  (do-applescript apscript))


;; use AppleScript to tell Safari to open URL at point
;; some code borrowed from browse-url-mozilla

(defun sw-safari-browse-url (url &optional new-window)
  "Open the url at or around point"
  (interactive (browse-url-interactive-arg "URL: "))
  (while (string-match "[,)]" url)
    (setq url (replace-match
               (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (setq apscript (format "
tell application \"Safari\"
	activate
	set URL of the first document to \"%s\"
end tell
" url))
  (do-applescript apscript))

(global-set-key [f10] `sw-safari-browse-url)


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




;; new fun do-applescript functions. Problem: the char Â is the Apple
;; line return thingie, Emacs chokes on it

(defvar sw-last-applescript nil
  "Stores the last Applescript command executed from Emacs.")

(defvar sw-applescript-buffer-name "*AppleScript output*"
  "Name for the buffer to display AppleScript output.")

(defun sw-applescript-run-buffer ()
  "Execute the whole buffer as an Applescript"
  (interactive)
  (setq sw-last-applescript (buffer-string))
  (sw-run-and-display-applescript (buffer-string)))

(defun sw-applescript-run-region ()
  "Execute the region as an Applescript"
  (interactive)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (setq sw-last-applescript region)
    (sw-run-and-display-applescript region)))

(defun sw-run-last-applescript ()
  "Run the last Applescript command again"
  (interactive)
  (sw-run-and-display-applescript sw-last-applescript))

(defun sw-run-and-display-applescript (code)
  "Switch to the AppleScript buffer, erase it, run the code and display the results."
  (switch-to-buffer (get-buffer-create sw-applescript-buffer-name))
  (erase-buffer)
  (insert (do-applescript code)))
