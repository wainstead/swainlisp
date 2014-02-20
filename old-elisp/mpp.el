(defun sw-to-mpp ()
  "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd ~swain/Sites/projects/mpa/myphotopro")
  (comint-send-input))

(defun sw-stop-rails () 
  (interactive)
  (switch-to-buffer (get-buffer "console"))
  (point-max)
  (insert "quit")
  (comint-send-input)
  
  (switch-to-buffer (get-buffer "mongrel"))
  (point-max)
  (comint-interrupt-subjob)
 )

(defun sw-move-rails-shells-here () 
  (interactive)

  (setq sw-current-dir (format "cd %s" (file-name-directory default-directory)))

  (switch-to-buffer (get-buffer "console"))
  (goto-char (point-max))
  (insert sw-current-dir)
  (comint-send-input)

  (switch-to-buffer (get-buffer "mongrel"))
  (goto-char (point-max))
  (insert sw-current-dir)
  (comint-send-input)

  (switch-to-buffer (get-buffer "cli"))
)
;; the big enchilada
(defun sw-mpp ()
  "Are we live or on tape?"
  (interactive)

  (setq sw-restore-shell-buffers-flag nil)

  (sw-shell "console")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/mpa/myphotopro")
  (comint-send-input)

  (sw-shell "mongrel")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/mpa/myphotopro")
  (comint-send-input)


  (sw-shell "cli")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/mpa/myphotopro")
  (comint-send-input)

  (sw-shell "sql")
  (goto-char (point-max))
  (insert "cd /Users/swain/Sites/projects/mpa/myphotopro")
  (comint-send-input)

  (setq sw-restore-shell-buffers-flag t)

  ;; We probably want to disable automatically saving the shell
  ;; buffers until this is decided by the user; I might walk away and
  ;; in the next few minutes all the shell buffer files would be
  ;; overwritten. Also, if I choose 'n' the old ones should be backed
  ;; up. Even better idea: if auto-save-desktop runs and I haven't
  ;; made the choice, automatically back them up.

  (if (y-or-n-p "Restore the shell buffers? ")
      (progn
        (sw-insert-saved-buffer-contents "cli")
        (sw-insert-saved-buffer-contents "mongrel")
        (sw-insert-saved-buffer-contents "console")
        (sw-insert-saved-buffer-contents "sql")
        )
    ;; else
    (progn
      (sw-backup-saved-buffer-contents "cli")
      (sw-backup-saved-buffer-contents "mongrel")
      (sw-backup-saved-buffer-contents "console")
      (sw-backup-saved-buffer-contents "sql")
      )
    )

  (sw-init-shell "mongrel" "script/server")
  (sw-init-shell "console" "script/console")
  (sw-init-shell "sql" "mysql -uroot myphotopro_development")
)
