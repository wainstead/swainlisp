;; Customizations, settings, whatnot for Ruby and Rails development

(add-hook 'ruby-mode-hook 'robe-mode)
;;(robe-mode)

;; Meddle with robe's keymap; cannot use the default settings until I
;; can run a proper ruby interpreter. Currently it fails for want of
;; some of our environment variables.
(define-key robe-mode-map (kbd "M-,") nil)
(define-key robe-mode-map (kbd "M-.") nil)
(define-key robe-mode-map (kbd "C-x ,") 'pop-tag-mark)

;; not until I get rails running locally for my app..
;;(define-key robe-mode-map (kbd "C-x .") 'robe-jump)

(define-key robe-mode-map (kbd "C-x .") 'xref-find-definitions)
