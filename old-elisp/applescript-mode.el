;; applescript-mode.el
;; basic mode for applescript files

;; $Id: applescript-mode.el,v 1.1.1.1 2004/07/16 21:20:24 swain Exp $

;; put this file in the library path, and these in .emacs:
;; (load-library "applescript-mode")
;; (add-to-list 'auto-mode-alist '("\\.scpt$" . applescript-mode))

(defconst applescript-mode-version "0.0.0.0.0.0.1"
  "applescript-mode version number, which cannot be small enough.")

(require 'font-lock)

;; start by defining the mode-hook variable
(defvar applescript-mode-hook nil
  "*List of functions to call when entering applescript mode.")

;; then the mode-map
(defvar applescript-mode-map nil
  "Local keymap for applescript mode buffers.")

;; here you can define the keys in the mode map
(if applescript-mode-map
    nil
  (setq applescript-mode-map (make-sparse-keymap))
  ;; (define-key applescript-mode-map keysequence command)

  )

;; beep and print an error message
(defun applescript-error-message (str)
  (beep)
  (message str)
  nil)


;; the command itself
(defun applescript-mode ()
  "Applescript mode, used for .applescript files in the Bluewire system. 
Really, this is just here so font-lock-mode can do syntax highlighting."
  (interactive)
  (kill-all-local-variables)
  
  (setq major-mode 'applescript-mode)
  (setq mode-name "AppleScript")
  (use-local-map applescript-mode-map)
  (run-hooks 'applescript-mode-hook)

  (defconst applescript-keywords
    (eval-when-compile
      (regexp-opt
       '("$class" "constraintString" "extraSelects" "orderBy" 
         "persistableNames" "saveOrder" "$scope" "selectFields"
         "$standalongclass") t))
    "applescript keywords.")

;;   (make-local-variable 'font-lock-defaults)
;;   (setq font-lock-defaults
;;  	'((applescript-font-lock-keywords-1
;;  	   applescript-font-lock-keywords-2
;;  	   ;; Comment-out the next line if the font-coloring is too
;;  	   ;; extreme/ugly for you.
;;  	   applescript-font-lock-keywords-3
;;  	   )
;;  	  nil				; KEYWORDS-ONLY
;;  	  T				; CASE-FOLD
;;  	  nil				; SYNTAX-ALIST
;;  	  nil				; SYNTAX-BEGIN
;;  	  (font-lock-syntactic-keywords . php-font-lock-syntactic-keywords)))

  
;;    ;; Fontify keywords
;;   (cons
;;    (concat "\\<\\(" applescript-keywords "\\)\\>")
;;    'font-lock-keyword-face)
  
  )

;; a few good functions
;; new fun do-applescript functions. Problem: the char Â is the Apple
;; line return thingie, Emacs chokes on it

(defvar sw-last-applescript nil
  "Stores the last Applescript command executed from Emacs.")

(defvar applescript-line-continue "\302"
  "The AppleScript line continuation character.")

(defun applescript-insert-line-cont ()
  "Insert the line continuation character"
  (interactive)
  (insert "\302"))

(defun sw-applescript-run-buffer ()
  "Execute the whole buffer as an Applescript"
  (interactive)
  (do-applescript (buffer-string)))

(defun sw-applescript-run-region ()
  "Execute the region as an Applescript"
  (interactive)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (setq sw-last-applescript region)
    (do-applescript region)))

(defun sw-run-last-applescript ()
  "Run the last Applescript command again"
  (interactive)
  (do-applescript sw-last-applescript))



(provide 'applescript)