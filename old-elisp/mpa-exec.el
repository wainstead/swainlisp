;; Pulling the "commands" out of mpa.el so it's a pure library

(setq sw-restore-shell-buffers-flag nil)
(sw-cli)
(sw-root)
(sw-www)
(sw-test)
(sw-sql)
(setq sw-restore-shell-buffers-flag t)

(mpa-init-shell "test" "cd ~/svn/pgms/perl")

(if (string= (getenv "USER") "root")
    (progn
      ;; will only work when emacs is run by root user
      (mpa-init-shell "cli" "su - swain")
      (mpa-init-shell "sql" "mpa")
      (mpa-init-shell "www" "su - www")
      ))
  ;; else we need to use sudo
;;  (progn
;;    ;;(sw-init-sudo)
;;    )
;;  )
