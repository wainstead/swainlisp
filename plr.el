;; MyPhotoAlbum specific things


(defvar sw-plr-hostname "polaroidstudio.com"
  "Default hostname for PLR stuff.")

(defun plr-subuser (subuser)
  "Call the stored procedure showuser(subuser)."
  (interactive "sSubuser name: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "call showuser('%s')\\G" subuser))
  (comint-send-input)
)

(defun plr-cart (sess-key)
  "This is my WILLIAM BENDIX memorial CORNER where I worship William
 Bendix like a GOD!!"
(interactive "sess_key (this is your osCsid cookie): ")
(switch-to-buffer "sql")
(goto-char (point-max))
(insert (format "SELECT * FROM osc_cart WHERE sess_key='%s'\\G" sess-key))
(comint-send-input))


(defun plr-user (user)
  "Look up an plr user by username."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sUsername: ")
  (insert (format "select * from users where username='%s' and service_id=2\\G select concat('http://', username, '.%s/') as link from users where service_id=2 and username='%s'; " user  sw-plr-hostname user))
  (comint-send-input))


(defun plr-banned (user)
  "Look up an plr user by username in the banlist."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sUsername: ")
  (insert (format "select * from banlist where username='%s'\\G" user))
  (comint-send-input))

(defun plr-ebanned (email)
  "Look up an plr user by email in the banlist."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sEmail: ")
  (insert (format "select * from banlist where email_address='%s'\\G" email))
  (comint-send-input))


(defun plr-adminme ()
  "pwn r00t"
  (interactive)
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert "update users set status=9 where username='swain';")
  (comint-send-input))

(defun plr-email (email)
  "Look up an plr user by email address."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sEmail address of user: ")
  (insert (format "select * from users where email_address='%s'\\G select concat('http://', username, '.myphotoalbum.com/') as link from users where email_address='%s'; select concat('http://www.myphotoalbum.com/app/signup/confirm.cgi?u=', username, '&uid=', userid) as activation_link from users where email_address='%s'; select concat('http://', username, '.myphotoalbum.com/ampira.redirect.php?uname=', username, '&password=', password) as login_link from users where email_address='%s';" email email email email))
  (comint-send-input))

(set-register ?u "update osc_orders set orders_status = 7, processing_state = 'ready' where orders_id in ()")


(defun plr-club (username)
  "Generate an update statement to update a club membership."
  (interactive "sEnter username: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "select @userid := userid from users where username = '%s'\\G" username))
  (insert "select * from mpa_club_memberships where mpa_userid=@userid\\G")
  (comint-send-input)
  (insert "update mpa_club_memberships set expire_date= where mpa_userid = @userid limit 1 \\G")
  (backward-char 38)
)


(defun plr-id (username)
  "Insert a sql SELECT statement to get the user's userid."
  (interactive "sUsername: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "select @id := userid from users where username='%s';" username))
  (comint-send-input)
)

(defun plr-lint ()
  "Run a lint check on the file the current buffer is visiting."
 (interactive)
 (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
   (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
   )
 )

(global-set-key [f5] 'plr-lint)
(defalias 'sw-lint 'plr-lint)




(defun sw-defeat-mandrake ()
  "defeat mandrake's broken security model."
  (interactive)
    (shell-command (format "chown swain %s" buffer-file-name))
    (shell-command (format "chmod a+r %s" buffer-file-name))
    )


(defun sw-x-defeat-mandrake ()
  "defeat mandrake's broken security model. make the file executable."
  (interactive)
    (shell-command (format "chown swain %s" buffer-file-name))
    (shell-command (format "chmod a+rx %s" buffer-file-name))
    )



(defun plr-make-clubber (username interval)
  "Turn a frog into a prince."
  (interactive "sUsername to upgrade to club: \nnNumber of years to make premium member: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "
BEGIN;
-- get their userid first. save in @id.
select @id := userid from users where username = '%s';

-- now create a new row in prem_packages_history. set the expiration
-- date for INTERVAL (365*N) DAY.
insert into prem_packages_history 
       (customer_id,  products_id,  date,   start_date,  expire_date)
VALUES (@id,          '148',        NOW(),  NOW(),       DATE_ADD( CURDATE( ) , INTERVAL (365*%d) DAY ));

-- now get the primary key just created for that user. save in @ptype.
select @ptype := packages_history_id from prem_packages_history where customer_id = @id;

-- finally, update the 'users' table setting package_type to the new key from prem_packages_history
UPDATE users SET status = '4',  package_type = @ptype WHERE userid = @id;

-- and confirm:
select * from users, prem_packages_history where userid=@id and package_type=packages_history_id and customer_id=@id\\G

" username interval))
  (comint-send-input)
  (message "Don't forget to type COMMIT or ROLLBACK!")
  )


(defun plr-send-backslash-g ()
  "meta return (M-RET) sends \G for mysql"
  (interactive)
  (insert "\\G")
  (comint-send-input)
  )
(global-set-key [(meta ?)] 'plr-send-backslash-g)



(defun plr-open-user-local-php (username)
  "Prompt for a username, open user.local.php"
  (interactive "sUsername: ")
  (plr-open-user-data-file username "user.local.php")
  )


(defun plr-open-config (username)
  "Prompt for a username, open config.php"
  (interactive "sUsername: ")
  (plr-open-user-data-file username "config.php")
  )


(defun plr-open-user-data-file (username filename)
  "Open the given filename in the user's data directory"
  (find-file (concat (plr-fast-storage-path) "/" (make-five-levels-subpath username 1) "/" filename)))



;;                    user_root: /d0/fast/plr01
;;                mass_storage: /d0/slow/plr01

(defun plr-fast-storage-path ()
  "return the fast storage path"
  "/d0/fast/plr01"
)

(defun plr-slow-storage-path ()
  "return the fast storage path"
  "/d0/slow/plr01"
)


(defun plr-open-fast (username)
  "Prompt for an album, open that user's fast1 album dir."
  (interactive "sUsername: ")
  (find-file (concat (plr-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums/")))


(defun plr-open-slow (username)
  "Prompt for an album, open that user's slow album dir."
  (interactive "sUsername: ")
  (find-file (concat (plr-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums/")))

(defun plr-open-album (username album)
  "Prompt for an album, open that album on both fast and slow."
  (interactive "sUsername: \nsAlbum: ")

    (find-file (concat (plr-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album))
    (split-window-vertically)
    (find-file (concat (plr-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album))
    (window-configuration-to-register ?d)
)


(defun plr-open-user-data-dirs (username)
  "Prompt for a user, open that user's fast and slow album dirs."
  (interactive "sUsername: ")
    (find-file (concat (plr-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
    (split-window-vertically)
    (find-file (concat (plr-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
    (window-configuration-to-register ?d)
    )
  

(defun plr-gallery ()
  "Jump to the Gallery installation for this server, in the shell where the user is www."
  (interactive)
  (switch-to-buffer (get-buffer "www"))
  (goto-char (point-max))
  (insert "cd /opt/plr_gallery/current")
  (comint-send-input))



(defun plr-cd-slow (username)
  "Prompt for a username, insert a change directory command at point-max."
  (interactive "sUsername: ")
  (if (string= major-mode "shell-mode")
      (progn
        (goto-char (point-max))
        (insert (concat "cd " (plr-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
        (comint-send-input))
    ;; else
    (message "This is not a shell buffer.")
    
    )
  )

(defun plr-cd-fast (username)
  "Prompt for a username, insert a change directory command at point-max."
  (interactive "sUsername: ")
  (if (string= major-mode "shell-mode")
      (progn
       (goto-char (point-max))
       (insert (concat "cd " (plr-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
       (comint-send-input))
    ;; else
    (message "This is not a shell buffer.")
    
    )
  )




;; move two shells to the "live" dir for the ofs.
(defun plr-ofs ()
  "I was in EXCRUCIATING PAIN until I started reading JACK AND JILL Magazine!!"
  (interactive)
  (switch-to-buffer (get-buffer "root"))
  (goto-char (point-max))
  (insert "cd /opt/mpa/dpi/ofs/bin")
  (comint-send-input)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd /opt/mpa/dpi/ofs")
  (comint-send-input))


(setq backup-by-copying t)


(defalias 'plr-sessiondb 'sw-sessiondb)


(defalias 'plr-storesessiondb 'sw-storesessiondb)


(defalias 'plr-cc-db 'sw-cc-db)






(defalias 'plr-init-shell 'sw-init-shell)


(defun plr-destroy-cart ()
  "Delete the cart for the oscsid under point. Put
   your cursor on the session ID in the osc log and run this function."
  (interactive)
  (let ( (plr-oscsid (thing-at-point 'word)) )
    (if (null plr-oscsid)
        (error "Point does not appear to be on an oscsid")
      )
    ;;(message (format "sid is %s" plr-oscsid))
    (switch-to-buffer "sql")
    (goto-char (point-max))
    (insert (format "call destroycart('%s');" plr-oscsid))
    (comint-send-input)
    )
  )

(defun plr-clear-discounts ()
  "Remove offers from my cart. Coupons too."
  (interactive)
  (let ( (plr-oscsid (thing-at-point 'word)) )
    (if (null plr-oscsid)
        (error "Point does not appear to be on an oscsid")
      )
    ;;(message (format "sid is %s" plr-oscsid))
    (switch-to-buffer "sql")
    (goto-char (point-max))
    (insert (format "call clearoffers('%s');" plr-oscsid))
    (comint-send-input)
    )
  )


(defun plr-format-an-response (response-string)
  "Format an Authorize.net response string into a readable way"
  (interactive "sResponse string: ")
  (switch-to-buffer (get-buffer-create "*Authorize.net Response*"))
  (erase-buffer)
  (plr-recurse-an-response 1 (split-string response-string "|"))
  (goto-char (point-min))
)

(defun plr-recurse-an-response (counter response-list)
  "recursively render the Authorize.net response"
  (if (not (null response-list))
      (progn
        (insert (format "%d: %s\n" counter (car response-list)))
        (plr-recurse-an-response (+ counter 1) (cdr response-list))
        )
    )
  )


(defun plr-highlight-items ()
  "...PENGUINS are floating by..."
  (interactive)
  (hi-lock-face-buffer "CC expiry" 'hi-green-b)
  (hi-lock-face-buffer "expired" 'hi-green-b)
  (hi-lock-face-buffer "[^ ]+ application_top started" 'hi-green-b)
  )


(defun plr-highlight-sessionkey ()
  "If elected, Zippy pledges to each and every American
 a 55-year-old houseboy..."
  (interactive)
  (let ( (plr-basket-id (thing-at-point 'word)) )
    (if (null plr-basket-id)
        (error "Point does not appear to be on a basket id")
      )
    (hi-lock-face-buffer plr-basket-id 'hi-red-b)
    )

  (plr-highlight-items)
  )


(defun plr-insert-svn ()
  "An INK-LING?  Sure -- TAKE one!!  Did you BUY any COMMUNIST UNIFORMS??"
  (interactive)
  (insert "https://svn.myphotoalbum.com/")
)
(global-set-key "\C-cs" 'plr-insert-svn)



(defun plr-mkcd (username pathfunc)
  "takes a username and either plr-fast-storage-path or
   plr-slow-storage-path. Returns the cd command."
  (concat "cd " (funcall pathfunc) "/" (make-five-levels-subpath username 1))
  )


(defun plrfast (username)
  (interactive "sUsername: ")
  (comint-goto-process-mark)
  (insert (plr-mkcd username 'plr-fast-storage-path))
  )

(defun plrslow (username)
  "Were these parsnips CORRECTLY MARINATED in TACO SAUCE?"
  (interactive "sUsername: ")
  (comint-goto-process-mark)
  (insert (plr-mkcd username 'plr-slow-storage-path))
  )
