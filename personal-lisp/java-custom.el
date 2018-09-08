;; Java customizations which are quite old. Moving them to a separate
;; file for tidyness.

;; larnep asked for this, so it is here. All tabs are spaces.
(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "cornflowerblue" "magenta" "cyan" "white"])
 '(ibuffer-saved-limits (quote (("java" ((name . ".java"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(indent-tabs-mode nil)
 '(line-number-display-limit nil)
 '(scroll-conservatively 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(show-paren-match-face ((((class color)) (:background "navy" :foreground "yellow"))))
 '(tnt-other-name-face ((((class color)) (:foreground "skyblue")))))

;; insert a javadoc comment
(fset 'insert-javadoc
      [?/ ?* ?* return ?* return ?* ?/ up ? ])

(defun sw-fb ()
  "Add a flowerbox to a Java file"
  (interactive)
  (let ( (projectdir (getenv "HOME")))
    ;; change the string here to the path off your $HOME
    (setq projectdir (concat projectdir "/projects/bluewire"))
    (insert
     (format "/** \n *\n */
" 
             ;; calculate the filename's path: full pathname minus projectdir
             (substring (buffer-file-name) (+ 1 (length projectdir))))))
  )


;; looks ugly, but generates a simple Exception class definition
(defun sw-except (name)
  "Insert a new Java Exception class based on user input"
  (interactive "sClass name: ")
  (insert (format "
/** 
  * @author $Author: swain $
  * @version $Id: swainlib.el,v 1.50 2007/11/13 16:29:59 swain Exp $
  * 
  */
class %s extends Exception {
    public %s() {}
    public %s(String msg) {
        super(msg);
    }
}" name name name))
  )

