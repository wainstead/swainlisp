;; Personal customizations for PHP
;; Removed from swainlib.el for tidyness

(add-to-list 'auto-mode-alist '("\\.tpl$"     . php-mode        ))

;; thanks Jeff Dairiki for this hook
(defun my-php-mode-hook-func ()
  (c-set-style "gnu")
  (setq tab-width 4
        c-basic-offset 4
        c-hanging-comment-ender-p nil
        indent-tabs-mode nil))

(add-hook 'php-mode-hook 'my-php-mode-hook-func)

(defun sw-php-lint ()
  "Run a lint check on the file the current buffer is visiting."
  (interactive)
  (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
    (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
    )
  )

(fset 'sw-php-lint-check-on-buffer
      [?\C-x ?h ?\M-| ?p ?h ?p ?  ?- ?l return])
