;; props-mode.el
;; basic mode for properties files

;; put this file in the library path, and these in .emacs:
;; (load-library "props-mode")
;; (add-to-list 'auto-mode-alist '("\\.properties$" . props-mode))

(defconst props-mode-version "0.0.0.0.0.0.1"
  "props-mode version number, which cannot be small enough.")

(require 'font-lock)

;; start by defining the mode-hook variable
(defvar props-mode-hook nil
  "*List of functions to call when entering .properties mode.")

;; then the mode-map
(defvar props-mode-map nil
  "Local keymap for properties mode buffers.")

;; here you can define the keys in the mode map
(if props-mode-map
    nil
  (setq props-mode-map (make-sparse-keymap))
  ;; (define-key props-mode-map keysequence command)

  )

;; beep and print an error message
(defun props-error-message (str)
  (beep)
  (message str)
  nil)


;; the command itself
(defun props-mode ()
  "Props mode, used for .properties files in the Bluewire system. 
Really, this is just here so font-lock-mode can do syntax highlighting."
  (interactive)
  (kill-all-local-variables)
  
  (setq major-mode 'props-mode)
  (setq mode-name ".properties")
  (use-local-map props-mode-map)
  (run-hooks 'props-mode-hook)

  (defconst props-keywords
    (eval-when-compile
      (regexp-opt
       '("$class" "constraintString" "extraSelects" "orderBy" 
         "persistableNames" "saveOrder" "$scope" "selectFields"
         "$standalongclass") t))
    ".properties keywords.")

;;   (make-local-variable 'font-lock-defaults)
;;   (setq font-lock-defaults
;;  	'((props-font-lock-keywords-1
;;  	   props-font-lock-keywords-2
;;  	   ;; Comment-out the next line if the font-coloring is too
;;  	   ;; extreme/ugly for you.
;;  	   props-font-lock-keywords-3
;;  	   )
;;  	  nil				; KEYWORDS-ONLY
;;  	  T				; CASE-FOLD
;;  	  nil				; SYNTAX-ALIST
;;  	  nil				; SYNTAX-BEGIN
;;  	  (font-lock-syntactic-keywords . php-font-lock-syntactic-keywords)))

  
;;    ;; Fontify keywords
;;   (cons
;;    (concat "\\<\\(" props-keywords "\\)\\>")
;;    'font-lock-keyword-face)
  
  )

(provide 'props)