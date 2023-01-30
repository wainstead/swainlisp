;; For the laptop's keyboard
(setq ns-right-option-modifier 'control)

(require 'package)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;(load-file ~spawlowski/.emacs.d/personal-lisp/sw-sql-map.el")
(load-file "~spawlowski/.emacs.d/personal-lisp/swainlib.el")
(load-file "~spawlowski/Documents/workfiles/lisp.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode t)
 '(column-number-mode t)
 '(desktop-path '("~"))
 '(display-time-mode t)
 '(explicit-shell-file-name "/opt/homebrew/bin/bash")
 '(markdown-command "pandoc")
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(org-tags-column -90)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(csharp-mode terraform-mode ox-gfm ox-slack sqlup-mode yasnippet-classic-snippets prettier-js eglot projectile-trailblazer react-snippets yasnippet-snippets projectile-rails flycheck auto-complete request lsp-mode gitignore-mode yasnippet csv-mode rspec-mode impatient-mode highlight-indentation highlight-indent-guides rubocop helm-robe robe ag typescript-mode groovy-mode markdown-mode magit dockerfile-mode yaml-mode json-mode projectile))
 '(require-final-newline nil)
 '(safe-local-variable-values '((eval visual-line-mode t)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-hunk-header ((((background dark)) (:foreground "Black" :background "#05057F7F8D8D")) (t (:foreground "White" :background "Salmon"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(diff-removed ((((background dark)) (:foreground "#7474FFFF7474")) (t (:foreground "DarkMagenta")))))

(put 'narrow-to-region 'disabled nil)
(setq-default tab-width 4)
(put 'set-goal-column 'disabled nil)

;;(load-file "~/.emacs.d/external-packages/load-external-packages.el")

