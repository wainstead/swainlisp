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
;; and this, for the laptop.
(setq mac-right-option-modifier 'control)
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
 '(org-agenda-files
   '("/Users/spawlowski/Documents/GTD/inbox.org"
	 "/Users/spawlowski/Documents/GTD/gtd.org"
	 "/Users/spawlowski/Documents/GTD/jira-tickets.org"
	 "/Users/spawlowski/Documents/GTD/someday.org"
	 "/Users/spawlowski/Documents/GTD/kanban-project.org"
	 "/Users/spawlowski/Documents/GTD/team-building.org"
	 "/Users/spawlowski/Documents/GTD/reference.org"
	 "/Users/spawlowski/.emacs.d/emacs.org"))
 '(safe-local-variable-values '((eval visual-line-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; M-x recentf-open-files to use this
(require 'recentf)
(recentf-mode 1)
;; Optional: Increase the number of saved files (default is often 20)
(setq recentf-max-saved-items 50)


;; Apr 2026: I have Claude writing to my log file sometimes, so let's
;; autosave my work and reload from disk when it changes. Courtesy
;; Gemini.

;; Save the actual file every 5 seconds of idle time
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;; Reload the file if it changed on disk
(global-auto-revert-mode 1)

(defun sw-insert-heading-timestamp ()
    "Insert a level-4 org heading with the current timestamp."
    (interactive)
    (insert (format-time-string "**** %a %b %-d %H:%M\n")))

(defun sw-insert-org-timestamp ()
    "Insert a level-4 org heading with the current timestamp."
    (interactive)
    (insert (format-time-string "**** [%Y-%m-%d %a %H:%M]\n")))

