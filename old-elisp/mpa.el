;; MyPhotoAlbum specific things


(defvar sw-my-hostname "myphotoalbum.com"
  "Default hostname for MPA stuff.")


(defun mpa-products-list ()
  "do that voodoo that you do"
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive)
  (insert "
-- list products and their relevant fields
SELECT DISTINCT p.products_id, 
 p.products_price, 
 pd.products_name,
 constant_name, 
 categories_id, 
 ofs_prefix_code,
 manufacturers_product_id
FROM
osc_products p,
osc_products_to_categories pc,
osc_products_description pd
WHERE p.products_id=pd.products_id 
AND  pc.products_id=pd.products_id
AND pc.categories_id=1
ORDER by p.products_id;

")
  (comint-send-input))

(defun mpa-subuser (subuser)
  "Call the stored procedure showuser(subuser)."
  (interactive "sSubuser name: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "call showuser('%s')\\G" subuser))
  (comint-send-input)
)

(defun mpa-cart (sess-key)
  "This is my WILLIAM BENDIX memorial CORNER where I worship William
 Bendix like a GOD!!"
(interactive "sess_key (this is your osCsid cookie): ")
(switch-to-buffer "sql")
(goto-char (point-max))
(insert (format "SELECT * FROM osc_cart WHERE sess_key='%s'\\G" sess-key))
(comint-send-input))

(defun mpa-cart-contents (sess_key)
  "Give unto me mine shopping cart contents, plebian."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sess_key (this is your osCsid cookie): ")
  (insert (format "
SELECT si.cart_id,
       cba.cart_contents_id, 
       cba.key,
       cba.value 
FROM
       osc_cart si, 
       osc_cart_contents cb,
       osc_cart_contents_attributes cba 
WHERE
       cba.cart_contents_id=cb.cart_contents_id AND
       cb.cart_id=si.cart_id AND
       si.sess_key='%s';
" sess_key))
  (comint-send-input))

;; http://www.myphotodevel.com/activate.php?u=suspectdevice&uid=143408&hash=
(defun mpa-user (user)
  "Look up an mpa user by username."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sUsername: ")
  (insert (format "select @userid := userid,  users.* from users where username='%s' and service_id=1\\G select concat('http://', username, '.%s/') as link from users where service_id=2 and username='%s'; " user  sw-my-hostname user))
  (comint-send-input))



(defun mpa-banned (user)
  "Look up an mpa user by username in the banlist."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sUsername: ")
  (insert (format "select * from banlist where username='%s'\\G" user))
  (comint-send-input))

(defun mpa-ebanned (email)
  "Look up an mpa user by email in the banlist."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sEmail: ")
  (insert (format "select * from banlist where email_address='%s'\\G" email))
  (comint-send-input))


(defun mpa-adminme ()
  "pwn r00t"
  (interactive)
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert "update users set status=9 where username='swain';")
  (comint-send-input))

(defun mpa-email (email)
  "Look up an mpa user by email address."
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sEmail address of user: ")
  (insert (format "select * from users where email_address='%s'\\G select concat('http://', username, '.myphotoalbum.com/') as link from users where email_address='%s'; select concat('http://www.myphotoalbum.com/app/signup/confirm.cgi?u=', username, '&uid=', userid) as activation_link from users where email_address='%s'; select concat('http://', username, '.myphotoalbum.com/ampira.redirect.php?uname=', username, '&password=', password) as login_link from users where email_address='%s';" email email email email))
  (comint-send-input))

(set-register ?u "update osc_orders set orders_status = 7, processing_state = 'ready' where orders_id in ()")


(defun mpa-club (username)
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


(defun mpa-id (username)
  "Insert a sql SELECT statement to get the user's userid."
  (interactive "sUsername: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (insert (format "select @id := userid from users where username='%s';" username))
  (comint-send-input)
)

(defun mpa-lint ()
  "Run a lint check on the file the current buffer is visiting."
 (interactive)
 (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
   (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
   )
 )

(global-set-key [f5] 'mpa-lint)
(defalias 'sw-lint 'mpa-lint)




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



(defun mpa-make-clubber (username interval)
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


(defun mpa-send-backslash-g ()
  "meta return (M-RET) sends \G for mysql"
  (interactive)
  (insert "\\G")
  (comint-send-input)
  )
(global-set-key [(meta ?)] 'mpa-send-backslash-g)



(defun mpa-open-user-local-php (username)
  "Prompt for a username, open user.local.php"
  (interactive "sUsername: ")
  (mpa-open-user-data-file username "user.local.php")
  )


(defun mpa-open-config (username)
  "Prompt for a username, open config.php"
  (interactive "sUsername: ")
  (mpa-open-user-data-file username "config.php")
  )


(defun mpa-open-user-data-file (username filename)
  "Open the given filename in the user's data directory"
  (find-file (concat (mpa-fast-storage-path) "/" (make-five-levels-subpath username 1) "/" filename)))


(defun make-five-levels-subpath (username counter)
  "Calls itself until a string is constructed in the form: s/sw/swa/swai/swain/swain"
  (if (= counter 6) username
  (concat (substring username 0 counter) "/" (make-five-levels-subpath username (+ counter 1))))
)

(defun mpa-fast-storage-path ()
  "return the fast storage path"
  "/mnt/fast/vol01"
)

(defun mpa-slow-storage-path ()
  "return the fast storage path"
  "/mnt/slow/vol01"
)


(defalias 'sw-open-config 'mpa-open-config)


(defun mpa-open-cel1 (username album)
  "Prompt for an album, open that user's cel1 album dir."
  (interactive "sUsername: \nsAlbum: ")
  (find-file (concat (mpa-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album)))

(defalias 'sw-open-cel1 'mpa-open-cel1)


(defun mpa-open-bob1 (username album)
  "Prompt for an album, open that user's bob1 album dir."
  (interactive "sUsername: \nsAlbum: ")
  (find-file (concat (mpa-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album)))

(defalias 'sw-open-bob1 'mpa-open-bob1)


(defun mpa-open-user-albums (username album)
  "Prompt for an album, open that album on both bob1 and cel1."
  (interactive "sUsername: \nsAlbum: ")

    (find-file (concat (mpa-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album))
    (split-window-vertically)
    (find-file (concat (mpa-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums/" album))
    (window-configuration-to-register ?d)
)


(defun mpa-open-user-data-dirs (username)
  "Prompt for a user, open that user's bob1 and cel1 album dirs."
  (interactive "sUsername: ")
    (find-file (concat (mpa-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
    (split-window-vertically)
    (find-file (concat (mpa-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
    (window-configuration-to-register ?d)
    )
  



(defalias 'sw-open-bob1 'mpa-open-bob1)


(defun mpa-gallery ()
  "Jump to the Gallery installation for this server, in the shell where the user is www."
  (interactive)
  (switch-to-buffer (get-buffer "www"))
  (goto-char (point-max))
  (insert "cd /opt/mpa_gallery/current")
  (comint-send-input))


(defun mpa-xml-explode ()
  "explode an xml file. That is, add linebreaks after every > char."
  (interactive)
  (sw-xml-prettyprint)
)


(defun mpa-order (orders_id)
  "Render the pertinent parts of an mpa order."
  (interactive "sorders_id: ")
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (window-configuration-to-register ?0)

  (insert (format "select * from osc_orders_products where orders_id=%s\\G" orders_id))
  (comint-send-input)

  (sleep-for 1) ;; give the output time to show up.

  (insert (format "select orders_products_id, products_options, products_options_values from osc_orders_products_attributes where orders_id=%s order by orders_products_id;" orders_id))
  (comint-send-input)
  )


(defun mpa-explode ()
  "Explode a serialized PHP data structure via replace-regexp"
  (interactive)
  (if buffer-read-only (toggle-read-only) )
  (save-excursion
    (point-min)
    (replace-regexp "\\([;\\}]\\)\\([Osib]\\):" "\\1
\\2")
    )
  )

(defalias 'sw-explode 'mpa-explode)


(defun mpa-cd-bob (username)
  "Prompt for a username, insert a change directory command at point-max."
  (interactive "sUsername: ")
  (if (string= major-mode "shell-mode")
      (progn
        (goto-char (point-max))
        (insert (concat "cd " (mpa-slow-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
        (comint-send-input))
    ;; else
    (message "This is not a shell buffer.")
    
    )
  )

(defun mpa-cd-cel (username)
  "Prompt for a username, insert a change directory command at point-max."
  (interactive "sUsername: ")
  (if (string= major-mode "shell-mode")
      (progn
       (goto-char (point-max))
       (insert (concat "cd " (mpa-fast-storage-path) "/" (make-five-levels-subpath username 1) "/albums"))
       (comint-send-input))
    ;; else
    (message "This is not a shell buffer.")
    
    )
  )




(defun sw-projects ()
  "A GRAM??  A BRAM...  A GROOM...  A BROOM...  Oh, Yeh!!  Wash the
 ROOM!!"
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd ~swain/public_html/projects")
  (comint-send-input))


(defun sw-gallery ()
  "Jump to the CVS copy of Gallery I'm currently working on.."
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd ~swain/public_html/projects/mpa_gallery/website")
  (comint-send-input))

(defun sw-signup ()
  "Jump to my copy of mpa_signup."
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (goto-char (point-max))
  (insert "cd ~swain/public_html/projects/mpa_signup")
  (comint-send-input))

(defun sw-swain ()
  "Stop laughing. It's not funny."
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd /mnt/cel-1/vol01/s/sw/swa/swai/swain/swain")
  (comint-send-input))

(defun sw-veracity ()
  "Now, let's SEND OUT for QUICHE!!"
  (interactive)
  (switch-to-buffer (get-buffer "www"))
  (goto-char (point-max))
  (insert "cd ~swain/public_html/projects/mpa_veracity")
  (comint-send-input)
)


(defun sw-gifts ()
  "Well, I'm a classic ANAL RETENTIVE!!  And I'm looking for a way to
 VICARIOUSLY experience some reason to LIVE!!"
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd /home/swain/public_html/projects/mpa_products")
  (comint-send-input))
 
 
(defun sw-cart ()
  "..  or were you driving the PONTIAC that HONKED at me in MIAMI last Tuesday?"
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd ~/public_html/projects/mpa_cart")
  (comint-send-input))

(defun sw-core ()
  "Yow!  I forgot my PAIL!!"
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd ~/public_html/projects/mpa_core")
  (comint-send-input))

(defun sw-schema ()
  "Look DEEP into the OPENINGS!!  Do you see any ELVES or EDSELS...
 or a HIGHBALL??..."
  (interactive)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd /home/swain/public_html/projects/schema/")
  (comint-send-input))

;; new: move two shells to the same dir. revolutionary in its obviousness.
(defun sw-ofs ()
  "I was in EXCRUCIATING PAIN until I started reading JACK AND JILL Magazine!!"
  (interactive)
  (switch-to-buffer (get-buffer "root"))
  (goto-char (point-max))
  (insert "cd /home/swain/public_html/projects/mpa_ofs/bin")
  (comint-send-input)
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "cd /home/swain/public_html/projects/mpa_ofs")
  (comint-send-input))


;; move two shells to the "live" dir for the ofs.
(defun mpa-ofs ()
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


(defun mpa-ftp ()
  "Somewhere in suburban Honolulu, an unemployed bellhop is whipping up
 a batch of illegal psilocybin chop suey!!"
  (interactive)
  (switch-to-buffer (get-buffer "root"))
  (goto-char (point-max))
  (insert "cd /opt/proftpd/var/ftp/pub")
  (comint-send-input)
)


;; replace ^M in an entire buffer. Needs to be reworked to do only a
;; region, eventually.
(fset 'sw-replace-M
   [?\M-< escape ?% ?\C-q ?\C-m return ?\C-q ?\C-j return ?!])


(setq backup-by-copying t)

(defun sw-fix-tail ()
  "Colorize the window that tails logs."
  (interactive)
  (set-default-font "-adobe-courier-medium-r-normal-*-*-120-*-*-*-*-iso8859-1")
  (set-background-color "#202020")
  (set-foreground-color "goldenrod")
)

(defalias 'sw-fix-logs 'sw-fix-tail) ;; because i keep forgetting

(fset 'sw-find-gallery-file
   [?\C-x ?\C-f ?\C-a ?\C-k ?/ ?h ?o ?m ?e ?/ ?s ?w ?a ?n ?i ?/ backspace backspace backspace ?i ?n ?/ ?p ?u ?b ?l ?i ?c ?_ ?p ?r backspace backspace ?h ?t ?m ?l ?/ ?p ?r ?o ?j ?e ?c ?t ?s ?/ ?a ?m ?p ?i ?r ?a ?d ?e ?v ?/ ?g ?a ?l ?l ?e ?r ?y ?/])

(defun sw-font ()
  "Fix the fracking font."
  (interactive)
  (set-default-font "-adobe-courier-medium-r-normal-*-*-120-*-*-*-*-iso8859-1")
  )

;; override default command for grep-find. I want to look at php files in Gallery mostly.
(setq grep-find-command "find . \\( -name \\*.php -o -name \\*.inc \\) -print0 | xargs -0 -e grep -n -e ")


(defun sw-test ()
  "Mmmmmm-MMMMMM!!  A plate of STEAMING PIECES of a PIG mixed
 with the shreds of SEVERAL CHICKENS!!...  Oh BOY!!  I'm
 about to swallow a TORN-OFF section of a COW'S LEFT LEG
 soaked in COTTONSEED OIL and SUGAR!!  ..  Let's see..
 Next, I'll have the GROUND-UP flesh of CUTE, BABY LAMBS
 fried in the MELTED, FATTY TISSUES from a warm-blooded
 animal someone once PETTED!!  ...  YUM!!  That was GOOD!!
 For DESSERT, I'll have a TOFU BURGER with BEAN SPROUTS
 on a stone-ground, WHOLE WHEAT BUN!!"
  (interactive)
  (let ( (test-buffer (get-buffer "test")) )
    (if (bufferp test-buffer)
        (switch-to-buffer test-buffer)
      ;; else create it                                                                                                              
      (progn
        (shell)
        (rename-buffer "test"))
      )
    )
  )

(defun sw-sessiondb ()
  "I'm having an emotional outburst!!"
  (interactive)
  (let ( (sessions-buffer (get-buffer "sessions")) )
    (if (bufferp sessions-buffer)
        (switch-to-buffer sessions-buffer)
      ;; else create it                                                                                                              
      (progn
        (shell)
        (rename-buffer "sessions")
        (insert "/d0/mysql/mpa/bin/mysql -S /tmp/mysql.session.sock  -uroot session")
        (comint-send-input))
      )
    )
  )

(defalias 'mpa-sessiondb 'sw-sessiondb)


(defun sw-storesessiondb ()
  "Do I hear th' SPINNING of various WHIRRING, ROUND, and WARM
 WHIRLOMATICS?!"
  (interactive)
  (let ( (sessions-buffer (get-buffer "store-sessions")) )
    (if (bufferp sessions-buffer)
        (switch-to-buffer sessions-buffer)
      ;; else create it                                                                                                              
      (progn
        (shell)
        (rename-buffer "store-sessions")
        (insert "/d0/mysql/mpa/bin/mysql -S /tmp/mysql.session.sock  -uroot session2")
        (comint-send-input))
      )
    )
  )

(defalias 'mpa-storesessiondb 'sw-storesessiondb)



(defun sw-cc-db ()
  "I'm having an emotional outburst!!"
  (interactive)
  (let ( (cc-db-buffer (get-buffer "cd db")) )
    (if (bufferp cc-db-buffer)
        (switch-to-buffer cc-db-buffer)
      ;; else create it                                                                                                              
      (progn
        (shell)
        (rename-buffer "cc db")
        (insert "/d0/mysql/mpa/bin/mysql -S /tmp/mysql.mpa.sock -uroot store_cc")
        (comint-send-input))
      )
    )
  )

(defalias 'mpa-cc-db 'sw-cc-db)


(defun sw-lint-pl ()
  "Run a lint check on the file the current buffer is visiting."
 (interactive)
 (let ( (php-interpreter "/usr/bin/perl -wc") ) 
   (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
   )
 )

(defun sw-lint-php ()
  "Run a lint check on the file the current buffer is visiting."
 (interactive)
 (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
   (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
   )
 )


(defun sw-lint-php5 ()
  "Run a lint check on the file the current buffer is visiting."
 (interactive)
 (let ( (php-interpreter "/opt/php5/bin/php -l") ) 
   (shell-command (format "%s %s" php-interpreter (buffer-file-name))) 
   )
 )

(global-set-key [f5] 'sw-lint-php5)




;; should factor out the alist used. pass it any alist, and
;; whammo. tailed logs.

(defvar sw-tail-store-frame-name "store logs" "Frame name for store logs")
(defvar sw-tail-store-alist '(
                             ("store log" . "/opt/mpa/logs/osc.log")
                             ("apache error2 log" . "/opt/apache2/logs/error_log")
                             )
  "List of store log files with names for buffers. Used by sw-tail-store-logs and sw-kill-store-logs.")

(defvar sw-tail-swainstore-frame-name "swainstore logs" "Frame name for the swain store logs")
(defvar sw-tail-swainstore-alist '(
                             ("swainstore osc log" . "/tmp/osc.log")
                             ;;("swainstore func log" . "/tmp/osc_func.log")
                             ("swainstore apache error2 log" . "/opt/apache2/logs/store_error_log")
                             )
  "List of swainstore log files with names for buffers. Used by sw-tail-store-logs and sw-kill-store-logs.")





(defun sw-tail-store-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-store-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-store-alist sw-tail-store-frame-name)
  (progn
    (select-frame-by-name sw-tail-store-frame-name)
    (sw-fix-logs)
    (sw-colors "200020")
    (set-frame-width (selected-frame) 250)
    (set-frame-height (selected-frame) 84)
    (enlarge-window -25)
    (window-configuration-to-register ?4)
    )
)

(defun sw-kill-store-logs ()
  (interactive)
  (sw-kill-logs-meta sw-tail-store-alist sw-tail-store-frame-name))




(defun sw-tail-swainstore-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-store-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-swainstore-alist sw-tail-swainstore-frame-name)
  (progn
    (select-frame-by-name sw-tail-swainstore-frame-name)
    (sw-fix-logs)
    (sw-colors "003030")
    (set-frame-width (selected-frame) 250)
    (set-frame-height (selected-frame) 84)
    (enlarge-window -25)
    (window-configuration-to-register ?2)
    )
  )

(defun sw-kill-swainstore-logs ()
  (interactive)
  (sw-kill-logs-meta sw-tail-swainstore-alist sw-tail-swainstore-frame-name))



(defun sw-tail-logs-meta (store-alist store-frame-name)
  "meta function for opening logs and tailing them in a new frame"
  ;; if we are on a windowing system like X11, open this in a new frame
  (if window-system
    (let ((logs-frame (make-frame)))
      (select-frame logs-frame)
      (set-frame-name store-frame-name)))

  (let (pair (file-alist store-alist))
    (while (consp file-alist) 
      ;; first time through these are equal so we do not split the buffer
      (if (not (equal (safe-length file-alist) (safe-length store-alist)))
          (split-window-vertically))
      (setq pair (car file-alist))
      (shell)
      (rename-buffer (car pair))
      (goto-char (point-max))
      (insert (format "tail -f `ls -t %s* | head -1`" (cdr pair)))
      (comint-send-input)
      ;;(message "car: %s cdr: %s" (car pair) (cdr pair))
      (setq file-alist (cdr file-alist))
      )
    (balance-windows)
    (window-configuration-to-register ?1))
  )
 
;; undo the work of sw-tail-logs-meta
(defun sw-kill-logs-meta (store-alist store-frame-name)
  "Kill the buffers tailing the log files as listed in store-alist."
  (if (y-or-n-p "Really kill the buffers that are tailing the log files? ")
      (progn
        (switch-to-buffer (car (car store-alist)))
        (delete-other-windows)
        (let ((file-alist store-alist))
          (while (consp file-alist)
            (setq pair (car file-alist))
            (unless (kill-buffer (car pair))
              (message (format "Couldn't kill the buffer %s." (car pair))))
            (setq file-alist (cdr file-alist))
            ))
        (when window-system
          (select-frame-by-name store-frame-name)
          (delete-frame))
        )
    ;; else:
        (message "Log tailing buffers not deleted.")))


(add-to-list 'auto-mode-alist '("gallery.conf$"     . php-mode))
(add-to-list 'auto-mode-alist '("store.conf$"       . php-mode))
(add-to-list 'auto-mode-alist '("store.swain.conf$" . php-mode))
(add-to-list 'auto-mode-alist '("default$"          . php-mode))


;;(fset 'flowerbox
  ;; [?/ ?* ?* tab return ?* tab return ?* ?/ tab up ? ])

(fset 'flowerbox
   "/**\C-m*\C-m*/\C-[OA ")


(fset 'spaceit
   " \C-f ")

(global-set-key [f10] 'spaceit)


;; spy on what user deleted, by appending to an html file (which we 
;; have to do, to get around hotlinking blocking via mod_rewrite)
;; sample url:
;; http://rawimages.myphotoalbum.com/c/ca/can/canu/canuc/canuckdaysfan/albums/album03/DSC02338.jpg
;; (defun sw-trashcan-append ()
;;   "append a url to the trashcan file"
;;   (interactive)
;;   (let ((path (buffer-substring (region-beginning) (region-end))))
;;     ;; the 'path' variable will now contain something like:
;;     ;; /mnt/bob-1/vol01/a/al/ale/alei/aleih/aleihah/albums/album01/DSCF0572.jpg
;;     ;; we want these components:
;;     ;; /a/al/ale/alei/aleih/aleihah, album01, DSCF0568.jpg
;;     (setq trashcan-append-me (concat "http://rawimages.myphotoalbum.com" (car (cdr (split-string path "vol01")))))
;;     (shell-command (format "echo '<a href=%s>%s</a><br>' >> /opt/mpa/custom-sites/nakedcowboy/xxx/trashcan.html" 
;;                            trashcan-append-me
;;                            trashcan-append-me)) 
;;     )
;;   )



(defalias 'mpa-init-shell 'sw-init-shell)

;; this has to be run when running Emacs as a normal user; otherwise
;; root can just su - to all the users itself.
(defun sw-init-sudo (passwd)
  "Prompt for password, execute a useless command"
  (interactive "sPassword:")

  ;; hack to seed the sudo command so following calls to sudo don't
  ;; need a password
  (shell-command (format "echo '%s' | sudo cat /dev/null" passwd))

  (mpa-init-shell "sql" "mpa")
  (mpa-init-shell "www" "sudo su - www")
  (mpa-init-shell "root" "sudo -s")

  (sw-insert-saved-buffer-contents "cli")
  (sw-insert-saved-buffer-contents "root")
  (sw-insert-saved-buffer-contents "sql")
  (sw-insert-saved-buffer-contents "test")
  (sw-insert-saved-buffer-contents "www")

  )


(defun mpa-destroy-cart ()
  "Delete the cart for the oscsid under point. Put
   your cursor on the session ID in the osc log and run this function."
  (interactive)
  (let ( (mpa-oscsid (thing-at-point 'word)) )
    (if (null mpa-oscsid)
        (error "Point does not appear to be on an oscsid")
      )
    ;;(message (format "sid is %s" mpa-oscsid))
    (switch-to-buffer "sql")
    (goto-char (point-max))
    (insert (format "call destroycart('%s');" mpa-oscsid))
    (comint-send-input)
    )
  )

(defun mpa-clear-discounts ()
  "Remove offers from my cart. Coupons too."
  (interactive)
  (let ( (mpa-oscsid (thing-at-point 'word)) )
    (if (null mpa-oscsid)
        (error "Point does not appear to be on an oscsid")
      )
    ;;(message (format "sid is %s" mpa-oscsid))
    (switch-to-buffer "sql")
    (goto-char (point-max))
    (insert (format "call clearoffers('%s');" mpa-oscsid))
    (comint-send-input)
    )
  )


(defun mpa-format-an-response (response-string)
  "Format an Authorize.net response string into a readable way"
  (interactive "sResponse string: ")
  (switch-to-buffer (get-buffer-create "*Authorize.net Response*"))
  (erase-buffer)
  (mpa-recurse-an-response 1 (split-string response-string "|"))
  (goto-char (point-min))
)

(defun mpa-recurse-an-response (counter response-list)
  "recursively render the Authorize.net response"
  (if (not (null response-list))
      (progn
        (insert (format "%d: %s\n" counter (car response-list)))
        (mpa-recurse-an-response (+ counter 1) (cdr response-list))
        )
    )
  )


(defun mpa-highlight-items ()
  "...PENGUINS are floating by..."
  (interactive)
  (hi-lock-face-buffer "CC expiry" 'hi-green-b)
  (hi-lock-face-buffer "expired" 'hi-green-b)
  (hi-lock-face-buffer "[^ ]+ application_top started" 'hi-green-b)
  )


(defun mpa-highlight-sessionkey ()
  "If elected, Zippy pledges to each and every American
 a 55-year-old houseboy..."
  (interactive)
  (let ( (mpa-basket-id (thing-at-point 'word)) )
    (if (null mpa-basket-id)
        (error "Point does not appear to be on a basket id")
      )
    (hi-lock-face-buffer mpa-basket-id 'hi-red-b)
    )

  (mpa-highlight-items)
  )


(defun mpa-insert-svn ()
  "An INK-LING?  Sure -- TAKE one!!  Did you BUY any COMMUNIST UNIFORMS??"
  (interactive)
  (insert "https://svn.myphotoalbum.com/")
)
(global-set-key "\C-cs" 'mpa-insert-svn)


(defun mpa-insert-svn-gallery ()
  "I am covered with pure vegetable oil and I am writing a best seller!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_gallery/")
)
(global-set-key "\C-cg" 'mpa-insert-svn-gallery)

(defun mpa-insert-svn-products ()
  "This is a NO-FRILLS flight -- hold th' CANADIAN BACON!!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_products/")
)
(global-set-key "\C-cp" 'mpa-insert-svn-products)

(defun mpa-insert-svn-cart ()
  "Yow!  Now we can become alcoholics!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_cart/")
)
(global-set-key "\C-cc" 'mpa-insert-svn-cart)

(defun mpa-insert-svn-ofs ()
  "Yow!  I'm UNEMPLOYED!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_ofs/")
)
(global-set-key "\C-co" 'mpa-insert-svn-ofs)

(defun mpa-insert-svn-core ()
  "Yow!  I'm UNEMPLOYED!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_core/")
)
;; 'r' for 'core' since 'c' is taken by 'cart'
(global-set-key "\C-cr" 'mpa-insert-svn-core)

(defun mpa-insert-svn-signup ()
  "Yow!  I'm UNEMPLOYED!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_signup/")
)
;; 'w' for 'wizard'
(global-set-key "\C-cw" 'mpa-insert-svn-core)

(defun mpa-insert-svn-veracity ()
  "Yow!  I'm UNEMPLOYED!"
  (interactive)
  (insert "https://svn.myphotoalbum.com/mpa_veracity/")
)
;; 'v' for 'veracity'
(global-set-key "\C-cv" 'mpa-insert-svn-veracity)

(defvar sw-tail-mpp-frame-name "myphotopro" "Frame name for mpp logs")
(defvar sw-tail-mpp-alist '(
                             ("production.log" . "/opt/myphotopro/current/log/production.log")
                             ("mongrel 8001" . "/opt/myphotopro/current/log/mongrel.8001.log")
                             ("mongrel 8002" . "/opt/myphotopro/current/log/mongrel.8002.log")
                             )
  "List of mpp log files with names for buffers. Used by sw-tail-mpp-logs.")

(defun sw-tail-mpp-logs ()
  "Tail log files in shell buffers. The files to tail, and the names to give
   to buffers, are in the alist sw-tail-store-alist."
  (interactive)
  (sw-tail-logs-meta sw-tail-mpp-alist sw-tail-mpp-frame-name)
  (progn
    (select-frame-by-name sw-tail-mpp-frame-name)
    (sw-fix-logs)
    (sw-colors "200020")
    (set-frame-width (selected-frame) 165)
    (set-frame-height (selected-frame) 70)
    (enlarge-window -25)
    (window-configuration-to-register ?5)
    )
)

(defun sw-kill-mpp-logs ()
  (interactive)
  (sw-kill-logs-meta sw-tail-mpp-alist sw-tail-mpp-frame-name))


(defun mkcd (username pathfunc)
  "takes a username and either mpa-fast-storage-path or
   mpa-slow-storage-path. Returns the cd command."
  (concat "cd " (funcall pathfunc) "/" (make-five-levels-subpath username 1))
  )


(defun mkcdfast (username)
  (interactive "sUsername: ")
  (comint-goto-process-mark)
  (insert (mkcd username 'mpa-fast-storage-path))
  )

(defun mkcdslow (username)
  "Were these parsnips CORRECTLY MARINATED in TACO SAUCE?"
  (interactive "sUsername: ")
  (comint-goto-process-mark)
  (insert (mkcd username 'mpa-slow-storage-path))
  )





(defun mpa-photo-info (photoid)
  "fully grok a photo"
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sPhoto ID: ")
  (insert (format "select u.username, pg.name, pp.filename, pp.ext, u.service_id from pro_portfolio_photos pp, pro_portfolio_gallery pg, pro_portfolio_gallery_photos pgp, users u where pp.id = %s and pp.id=pgp.photo_id and pgp.gallery_id = pg.id and pp.photog_id = u.userid;" photoid))
  (comint-send-input))


(defun mpa-album-info (galleryid)
  "fully grok an album"
  (switch-to-buffer "sql")
  (goto-char (point-max))
  (interactive "sGallery/Album ID: ")
  (insert (format "select username, name, service_id from users u, pro_portfolio_gallery pg where userid=owner_id and id = %s;" galleryid))
  (comint-send-input))


(defun sw-update-builds ()
  "update all of my builds on torque for me"
  (switch-to-buffer (get-buffer "cli"))
  (goto-char (point-max))
  (insert "updateall")
  (comint-send-input))

(global-set-key (kbd "<f2> u") (lambda () (interactive) (sw-update-builds)))

(defun gf-php (pattern)
  "Search store project for PHP files containing REGXP"
  (interactive "sEnter search string: ")
  ;; save the current working directory for this buffer
  ;; using cd commands resets default-directory apparently
  (setq current-dir default-directory)
  (cd "/home/swain/public_html/projects/ampiradev/gallery-sc/public_html")
  (grep-find 
   (concat "find . \\( -name \\*.php -o -name \\*.inc \\) -print0 | xargs -0 -e grep -n -e " pattern))

  ;; go back to the right working dir
  (cd-absolute current-dir))

(set-register ?u "update osc_orders set orders_status = 7, processing_state = 'ready' where orders_id in ()")


