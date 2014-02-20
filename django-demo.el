;; for the django demo
(load-file "~swain/.elisp/ipython.el")

(defun sw-django ()
  "thine own self be this"
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/Sites/projects/python-meetups/django/")
  (comint-send-input))

;; set up stuff for sql-sqlite
(setq sql-sqlite-program "/opt/local/bin/sqlite3")
(setq sql-database "/tmp/dbdemo.db")

