(defun insert-tabs-hook-func ()
  "Set up indentation for SSC, which prefers tabs for indentation."
  (setq indent-tabs-mode t)
  (setq tab-width 4)
)
(add-hook 'python-mode-hook 'insert-tabs-hook-func)
(add-hook 'sql-mode-hook 'insert-tabs-hook-func)


(setq compile-command "cd ~swain/git/pippin; make nfmc")

(blink-cursor-mode 0)
(transient-mark-mode t)
(set-mouse-color "white")

(put 'narrow-to-region 'disabled nil)
(setq mac-command-modifier 'meta)

(defun sw-pippin ()
  "jump to the top"
  (interactive)
  (switch-to-buffer "cli")
  (goto-char (point-max))
  (insert "cd ~/git/pippin")
  (comint-send-input))

(defun sw-psql ()
  "Open a shell buffer, rename it 'psql'"
  (interactive)
  (sw-shell "psql")
  (goto-char (point-max))
  (insert "psql")
  (comint-send-input)
  (insert "set search_path=nfmc,public;")
  (comint-send-input)
  )

(defun sw-start.dev ()
  "Open a shell buffer, rename it 'start.dev'"
  (interactive)
  (sw-shell "start.dev")
  (goto-char (point-max))
  )


(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  )

