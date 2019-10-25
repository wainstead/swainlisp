;; Database stuff
(defvar sw-sql-map nil
  "Steve Wainstead's personal keymap for SQL/database commands")
(define-prefix-command 'sw-sql-map)
;; 's' is for 'SQL'
(global-set-key "\M-as" 'sw-sql-map)

;; Thus far this effort fails. I want to use one function for handling
;; the details of sending the contents of the register to the buffer,
;; but passing something like ?s is problematic via the lambda
;; function. Note, however, sometimes I want to call comint-send-input
;; and sometimes I don't, so that needs to be taken into
;; consideration.
;; (defun sw-send-command-to-psql (register-char)
;;   "Insert the contents of a register at max-point"
;;   (interactive)
;;      (goto-char (point-max))
;;      (insert-register (byte-to-string register-char))
;;      (comint-send-input))
;; (define-key sw-sql-map "c"
;;   '(lambda () (sw-send-command-to-psql ?s)))
;; (define-key sw-sql-map "t"
;;   '(lambda () (message (char ?t))))



;; mnemonic: "c" for "connect"
;; (define-key sw-sql-map "c"
;;   '(lambda ()
;;      "Insert contents of register 'c' at point-max"
;;      (interactive)
;;      (goto-char (point-max))
;;      (insert-register ?s)
;;      (comint-send-input)))

;; ;; mnemonic: "t" for transaction. I used "l" originally because I
;; ;; didn't have any better registers available.
;; (define-key sw-sql-map "t"
;;   '(lambda ()
;;      "Insert contents of register 'l' at point-max, which inserts
;; the SQL to start a transaction that calls
;; nfmc.set_current_user_and_ip."
;;      (interactive)
;;      (goto-char (point-max))
;;      (insert-register ?l)
;;      (goto-char (point-max))
;;      (backward-word)
;;      ))

;; ;; mnemonic: "a" for audit log.
;; (define-key sw-sql-map "a"
;;   '(lambda ()
;;      "Insert contents of register 'a' at point-max, which inserts
;; the SQL to select the most recent lines from nfmc.audit_log."
;;      (interactive)
;;      (goto-char (point-max))
;;      (insert-register ?a)
;;      (goto-char (point-max))
;;      (backward-word)
;;      ))

;; ;; mnemonic: "r" for reload
;; (define-key sw-sql-map "r"
;;   '(lambda ()
;;      "Insert the command to reload the database into the psql shell"
;;      (interactive)
;;      (goto-char (point-max))
;;      (insert "\\i ~/sql/reloaddb.sql")
;;      (comint-send-input)
;;      ))
