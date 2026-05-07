;; Core packages that load automatically: straight.el bootstrap, god-mode, key-chord, alfred-buffers

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and load core packages
(straight-use-package 'god-mode)
(straight-use-package 'key-chord)

(require 'key-chord)
(key-chord-mode 1)

;; alfred-buffers is a local file not on MELPA; load directly
(load-file (expand-file-name "external-packages/alfred-buffers.el" user-emacs-directory))

(provide 'packages-core)
