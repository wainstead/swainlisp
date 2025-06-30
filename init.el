;; Minimal init.el for debugging
(require 'package)

;; Add MELPA repository
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; Initialize package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install and configure key-chord
(unless (package-installed-p 'key-chord)
  (package-install 'key-chord))
(require 'key-chord)
(key-chord-mode 1)

;; Basic settings
(show-paren-mode t)
(column-number-mode t)
(setq-default tab-width 4)

;; macOS specific settings
(setq ns-command-modifier 'meta)  ;; Set command key as meta
(setq ns-alternate-modifier 'alt)  ;; Keep option/alt key as alt

;; Load personal configuration
(load-file "~/.emacs.d/personal-lisp/swainlib.el")

;; Everything else is commented out for debugging
;; (setq ns-right-option-modifier 'control)
;; (setq insert-directory-program "gls" dired-use-ls-dired t)
;; (setq dired-listing-switches "-al --group-directories-first")
;; (load-file "~/Documents/workfiles/lisp.el")

;; Commented out custom-set-variables and custom-set-faces
;; (custom-set-variables ...)
;; (custom-set-faces ...) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval visual-line-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
