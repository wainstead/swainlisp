;;; p-whim-lock.el --- Minor mode for interactive automatic highlighting.


;; Author: David M. Koppelman, koppel@ee.lsu.edu

;; hacked extensively to add nice colors for emacs with a black background.

;;; Commentary
;; 
;;  With the p-whim-lock commands text matching interactively entered
;;  regexp's can be highlighted.  For example, `M-x highlight-regexp
;;  RET clearly RET RET' will highlight all occurrences of `clearly'
;;  using a yellow background face.  New occurrences of `clearly' will
;;  be highlighted as they are typed.  `M-x unhighlight-regexp RET'
;;  will remove the highlighting.  Any existing face can be used for
;;  highlighting and a set of appropriate faces is provided.  The
;;  regexps can be written into the current buffer in a form that will
;;  be recognized the next time the corresponding file is read.
;;
;;  An updated version of this package (renamed hi-lock) will be included
;;  in Emacs 21.
;;
;;  Applications:
;;
;;    In program source code highlight a variable to quickly see all
;;    places it is modified or referenced:
;;    M-x highlight-regexp ground_contact_switches_closed RET RET
;;
;;    In a shell or other buffer that is showing lots of program
;;    output, highlight the parts of the output you're interested in:
;;    M-x highlight-regexp Total execution time [0-9]+ RET hi-blue-b RET
;;
;;    In buffers displaying tables, highlight the lines you're interested in:
;;    M-x highlight-lines-matching-regexp January 2000 RET hi-black-b RET
;;
;;    When writing text, highlight personal cliches.  This can be
;;    amusing.
;;    M-x highlight-regexp as can be seen RET RET
;;

;; Setup:
;;
;; Put the following code in your .emacs file, possibly changing
;; the key bindings in the global-set-key functions.
;;
;; (if window-system
;;     (progn
;;       (require 'p-whim-lock)
;;       (global-set-key "\C-xwi" 'p-whim-lock-find-patterns)
;;       (global-set-key "\C-xwh" 'p-whim-lock-face-buffer)
;;       (global-set-key "\C-xwr" 'p-whim-lock-unface-buffer)
;;       (global-set-key "\C-xwb" 'p-whim-lock-write-interactive-patterns)))
;;
;; Re-start Emacs or evaluate the added code.
;;
;; Go to a buffer in which font-lock is turned on.
;;
;; To highlight all occurrences of the word "the" type \C-xwh the RET RET.
;; To remove highlighting type \C-xwr. 
;; To save highlight patterns at the point type \C-xwb.
;; To re-read saved patterns type \C-xwi.


;; Whim-lock: (("\\<\\(setq\\|interactive\\|list\\|intern\\|mapcar\\|lambda\\)\\>" . font-lock-keyword-face))
;; Whim-lock: ( ("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))))
;; Whim-lock: end

(defvar whim-lock-mode nil
  "Whim lock mode.")

(defvar p-whim-lock-file-keywords nil
  "Keywords found in file for whim lock.")

(defvar p-whim-lock-original-keywords nil
  "Saved font lock keywords, used whenever whim-lock keywords change.")

(defvar p-whim-lock-exclude-modes
  '(rmail-mode mime/viewer-mode)
  "List of major modes in which whim lock will not run.
Perhaps for security reasons.")

(defvar p-whim-lock-interactive-keywords nil
  "Keywords provided to whim-lock by user.")

(defvar p-whim-lock-face-history
      (list "bwl-liteblu" "bwl-pink" "bwl-red" "bwl-green" "bwl-blue" 
	    "hi-yellow" "hi-blue" "hi-pink" "hi-green" "bwl-black")
      "History list of faces for whim-lock interactive functions.")

(defvar p-whim-lock-regexp-history nil
  "History of regexps used for interactive fontification.")

(make-variable-buffer-local 'p-whim-lock-original-keywords)
(make-variable-buffer-local 'p-whim-lock-interactive-keywords)
(put 'p-whim-lock-interactive-keywords 'permanent-local t)
(make-variable-buffer-local 'p-whim-lock-regexp-history)
(put 'p-whim-lock-regexp-history 'permanent-local t)
(make-variable-buffer-local 'p-whim-lock-file-keywords)
(put 'p-whim-lock-file-keywords 'permanent-local t)
(make-variable-buffer-local 'whim-lock-mode)
(put 'whim-lock-mode 'permanent-local t)

(if window-system 
    (progn
      (copy-face 'default 'hi-yellow)
      (set-face-background 'hi-yellow "yellow")
      (copy-face 'default 'hi-pink)
      (set-face-background 'hi-pink "pink")
      (copy-face 'default 'hi-green)
      (set-face-background 'hi-green "green")
      (copy-face 'default 'hi-blue)
      (set-face-background 'hi-blue "light blue")
      (copy-face 'default 'bwl-black)
      (make-face-bold 'bwl-black)

      (copy-face 'default 'bwl-blue)
      (set-face-foreground 'bwl-blue "blue")
      (make-face-bold 'bwl-blue)

      ;; attempt to add new colors
      (copy-face 'default 'bwl-liteblu)
      (set-face-foreground 'bwl-liteblu "light blue")
      (make-face-unbold 'bwl-liteblu)

      (copy-face 'default 'bwl-yellow)
      (set-face-foreground 'bwl-yellow "yellow")
      (make-face-unbold 'bwl-yellow)

      (copy-face 'default 'bwl-pink)
      (set-face-foreground 'bwl-pink "pink")
      (make-face-unbold 'bwl-pink)

      (copy-face 'default 'bwl-green)
      (set-face-foreground 'bwl-green "green")
      (make-face-unbold 'bwl-green)

      (copy-face 'default 'bwl-red)
      (set-face-foreground 'bwl-red "red")
      (make-face-unbold 'bwl-red)))

(or (assq 'whim-lock-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(whim-lock-mode " W") minor-mode-alist)))


;; Visible Functions
(defalias 'highlight-lines-matching-regexp 'p-whim-lock-line-face-buffer)
(defun p-whim-lock-line-face-buffer (regexp &optional face)
  "Set face of all lines containing matches of REGEXP to FACE."
  (interactive
   (list
    (read-from-minibuffer "Regexp to highlight line: "
			  (cons (or (car p-whim-lock-regexp-history) "") 1 )
			  nil nil 'p-whim-lock-regexp-history)
    (intern (completing-read "Highlight using face: "
			  obarray 'facep t (car p-whim-lock-face-history)
			  '(p-whim-lock-face-history . 0)))))
  (if (or (not (stringp regexp)) (= (length regexp) 0))
      (error "Invalid regexp.")) 
  (if (not whim-lock-mode) (whim-lock-mode))
  (or (facep face) (setq face 'hi-yellow))
  (add-to-list 'p-whim-lock-interactive-keywords 
               (list (concat "^.*" regexp ".*$") (list 0 (list 'quote face) t)))
  (p-whim-lock-set-keywords))

(defalias 'highlight-regexp 'p-whim-lock-face-buffer)
(defun p-whim-lock-face-buffer (regexp &optional face)
  "Set face of all matches of REGEXP to FACE."
  (interactive
   (list
    (read-from-minibuffer "Regexp to highlight: "
			  (cons (or (car p-whim-lock-regexp-history) "") 1 )
			  nil nil 'p-whim-lock-regexp-history)
    (intern (completing-read "Highlight using face: "
                             obarray 'facep t (car p-whim-lock-face-history)
                             '(p-whim-lock-face-history . 0)))))
  (if (or (not (stringp regexp)) (= (length regexp) 0))
      (error "Invalid regexp.")) 
  (or (facep face) (setq face 'hi-yellow))
  (if (not whim-lock-mode) (whim-lock-mode))
  (add-to-list 'p-whim-lock-interactive-keywords 
               (list regexp (list 0 (list 'quote face) t)))   
  (p-whim-lock-set-keywords))

(defalias 'unhighlight-regexp 'p-whim-lock-unface-buffer)
(defun p-whim-lock-unface-buffer (regexp)
  "Remove faces set to matches of REGEXP by whim-lock."
 (interactive
  (let
      ((history-list (mapcar (lambda (p) (car p)) 
			     p-whim-lock-interactive-keywords)))
   (list
    (completing-read "Regexp to unhighlight: "
		     p-whim-lock-interactive-keywords
		     t
		     t
		     (car (car p-whim-lock-interactive-keywords))
		     (cons 'history-list 1)))))
 (setq p-whim-lock-interactive-keywords
       (delq 
	(assoc regexp p-whim-lock-interactive-keywords)
	p-whim-lock-interactive-keywords))
 (p-whim-lock-set-keywords))

(defun p-whim-lock-write-interactive-patterns ()
  "Write interactive patterns, if any, into buffer at point."
  (interactive)
  (mapcar 
   (lambda (pattern)
     (insert (format "%s Whim-lock: (%s) %s\n" 
		     (or comment-start "")
		     (prin1-to-string pattern)
		     (or comment-end ""))))
   p-whim-lock-interactive-keywords))

(defun whim-lock-mode (&optional arg)
  "Toggle minor mode for adding per-buffer font-lock patterns.
If ARG positive turn whim-lock on.

\\[p-whim-lock-face-buffer] REGEXP FACE 
  Highlight matches of REGEXP with FACE.

\\[p-whim-lock-line-face-buffer] REGEXP FACE 
  Highlight lines continain matches of REGEXP with FACE.

\\[p-whim-lock-unface-buffer] REGEXP 
  Remove highlighting on matches of REGEXP. 

\\[p-whim-lock-write-interactive-patterns] 
  Write active REGEXPs into buffer as comments (if possible). They
will be used next time file is loaded.

\\[p-whim-lock-find-patterns] 
  Re-read patterns stored in buffer (in format produced by
\\[p-whim-lock-write-interactive-patterns]).
 
When font lock started beginning of file searched for:
 Whim-lock: FOO
where FOO is a list of patterns. These are added to the font lock keywords
already present."
  (interactive)
  (let ((whim-lock-mode-prev whim-lock-mode))
    (setq whim-lock-mode
           (if (null arg) (not whim-lock-mode)
             (> (prefix-numeric-value arg) 0)))
    ;; Turned on.
    (if (and (not whim-lock-mode-prev) whim-lock-mode)
        (progn
          (if (not font-lock-mode) (turn-on-font-lock))
          (p-whim-lock-find-patterns)))
    ;; Turned off.
    (if (and whim-lock-mode-prev (not whim-lock-mode))
        (progn
          ;; Keep keywords.
          (setq font-lock-keywords p-whim-lock-original-keywords)
          (p-whim-lock-refontify)
          ))))

(defun whim-lock-find-file-hook ()
  "Turn on whim lock if patterns present."
  (p-whim-lock-find-patterns))

;; Support Functions

(defun p-current-line (&optional end)
  "Return line number of point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (or end (point))))))

;; Implementation Functions

(defun p-whim-lock-set-keywords (&optional reinit)
  "Add whim-lock's interactive and file keywords to font-lock keywords."
  (if (or reinit (null p-whim-lock-original-keywords))
      (setq p-whim-lock-original-keywords (or font-lock-keywords '(t))))
  (setq font-lock-keywords 
	(append (delq t p-whim-lock-original-keywords)
                p-whim-lock-file-keywords
                p-whim-lock-interactive-keywords))
  (p-whim-lock-refontify))

(add-hook 'font-lock-mode-hook 'p-whim-lock-grab-keywords)
(add-hook 'find-file-hooks 'whim-lock-find-file-hook)

(defun p-whim-lock-grab-keywords ()
  "If whim lock on, save font lock's keywords."
  (if whim-lock-mode (progn (p-whim-lock-set-keywords t))))

(defun p-whim-lock-refontify ()
  "Unfontify and refontify buffer. Used when whim-lock patterns change."
  (interactive)
  (if (not font-lock-mode) (font-lock-mode))
  (font-lock-unfontify-buffer)
  (cond
   ;; Not doing anything seems to work in 19.32 and 20.3.2.  I
   ;; wonder if I ever really needed to refontify.
   (nil nil)
   ;; Need a better way, since this assumes too much about lazy lock.
   (lazy-lock-mode
    (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)))
      (while windows
;	  (lazy-lock-fontify-conservatively (car windows))
        (lazy-lock-fontify-window (car windows))
        (setq windows (cdr windows)))))
   (t (font-lock-fontify-buffer))))

(defun p-whim-lock-find-patterns ()
  "Find patterns in current buffer for whim lock."
  (interactive)
  (or
   (memq major-mode p-whim-lock-exclude-modes)
   (let ((all-patterns nil))
     (save-excursion
       (widen)
       (goto-char (point-min))
       (while
           (and
            (re-search-forward "\\<Whim-lock: " (+ (point) 1000) t)
            (not (looking-at "end")))
         (let
             ((patterns
               (condition-case nil
                   (read (current-buffer))
                 (error  (message 
                          (format "Could not read expression at %d" 
                                  (p-current-line)))
                         nil))))
           (if patterns
               (setq all-patterns
                     (append patterns all-patterns))))))
     (if (and (not whim-lock-mode) all-patterns)
         (whim-lock-mode 1))
     (if whim-lock-mode
         (progn
           (setq p-whim-lock-file-keywords all-patterns)
           (p-whim-lock-set-keywords))))))

(provide 'p-whim-lock)
