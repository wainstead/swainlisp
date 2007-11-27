;; Pulling the "commands" out of mpa.el so it's a pure library
;; $Id: mpa-exec.el,v 1.1 2007/04/17 19:55:49 swain Exp $

(sw-cli)
(sw-root)
(sw-www)
(sw-cvssync)
(sw-test)
(sw-sql)


(if (string= (getenv "USER") "root")
    (progn
      ;; will only work when emacs is run by root user
      (mpa-init-shell "cli" "su - swain")
      (mpa-init-shell "sql" "mpa")
      (mpa-init-shell "www" "su - www")
      (mpa-init-shell "cvssync" "su - cvssync")
      ))
  ;; else we need to use sudo
;;  (progn
;;    ;;(sw-init-sudo)
;;    )
;;  )
