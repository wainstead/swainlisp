;;; pg.lisp -- socket level interface to the PostgreSQL RDBMS for Common Lisp
;;
;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;; Time-stamp: <2001-01-18 emarsden>
;; Version: 0.4
;;
;;     Copyright (C) 1999,2000,2001  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>
;; The latest version of this package should be available from
;;
;;     <URL:http://www.chez.com/emarsden/downloads/>


;;; Overview =========================================================
;;
;; This module lets you access the PostgreSQL object-relational DBMS
;; from Common Lisp. The code implements the client part of the
;; socket-level frontend/backend protocol, rather than providing a
;; wrapper around the libpq library. The module is capable of type
;; coercions from a range of SQL types to the equivalent Lisp type.
;; The only non portable code is the use of 'socket-connect' and
;; (optional) some way of accessing the Unix crypt() function.
;;
;; Works with CMUCL, CLISP, ACL, perhaps CormanLisp. Lispworks port
;; partially finished.


;;; Entry points =======================================================
;;
;; (pg-connect dbname user &key password host port) -> connection
;;     Connect to the database DBNAME on HOST (defaults to localhost)
;;     at PORT (defaults to 5432) via TCP/IP and log in as USER. If
;;     the database requires a password, send PASSWORD (as clear text
;;     unless the backend demands crypt() authentication). Set the
;;     output date type to 'ISO', and initialize our type parser
;;     tables.
;;
;; (pg-exec connection &rest sql) -> pgresult
;;     Concatenate the SQL strings and send to the backend. Retrieve
;;     all the information returned by the database and return it in
;;     an opaque record PGRESULT.
;;
;; (pg-result pgresult what &rest args) -> info
;;     Extract information from the PGRESULT. WHAT can be one of 
;;          * :connection
;;          * :status
;;          * :attributes
;;          * :tuples
;;          * :tuple tupleNumber
;;          * :oid
;;     `connection' allows you to retrieve the database connection.
;;     `status' is a string returned by the backend to indicate the
;;     status of the command; it is normally "SELECT" for a select
;;     command, "DELETE 1" if the deletion affected a single row, etc.
;;     `attributes' is a list of tuples providing metadata: the first
;;     component of each tuple is the attribute's name as a string,
;;     the second an integer representing its PostgreSQL type, and the
;;     third an integer representing the size of that type. `tuples'
;;     returns all the data retrieved from the database, as a list of
;;     lists, each list corresponding to one row of data returned by
;;     the backend. `tuple num' can be used to extract a specific
;;     tuple. `oid' allows you to retrieve the OID returned by the
;;     backend if the command was an insertion; the OID is a unique
;;     identifier for that row in the database (this is
;;     PostgreSQL-specific, please refer to the documentation for more
;;     details).
;;
;; (pg-disconnect connection) -> nil
;;     Close the database connection.
;;
;; Boolean variable `PG-DISABLE-TYPE-COERCION' which can be set to non-nil
;; (before initiating a connection) to disable the library's type
;; coercion facility. Default is t.
;;
;;
;;
;; SECURITY NOTE: setting up PostgreSQL to accept TCP/IP connections
;; has security implications; please consult the documentation for
;; details. pg.lisp is able to use the crypt authentication method to
;; avoid sending the password in cleartext over the wire (this assumes
;; access to the `crypt' function via the FFI). It does not support
;; the Kerberos authentication method. However, it is possible to use
;; the port forwarding capabilities of ssh to establish a connection
;; to the backend over TCP/IP, which provides both a secure
;; authentication mechanism and encryption (and optionally
;; compression) of data passing through the tunnel. Here's how to do
;; it (thanks to Gene Selkov, Jr. <selkovjr@mcs.anl.gov> for the
;; description):
;;
;; 1. Establish a tunnel to the backend machine, like this:
;; 
;; 	ssh -L 3333:backend.dom:5432 postgres@backend.dom
;; 
;;    The first number in the -L argument, 3333, is the port number of
;;    your end of the tunnel. The second number, 5432, is the remote
;;    end of the tunnel -- the port number your backend is using. The
;;    name or the address in between the port numbers belongs to the
;;    server machine, as does the last argument to ssh that also includes
;;    the optional user name. Without the user name, ssh will try the
;;    name you are currently logged on as on the client machine. You can
;;    use any user name the server machine will accept, not necessarily
;;    those related to postgres.
;; 
;; 2. Now that you have a running ssh session, you can point pg.lisp to
;;    the local host at the port number which you specified in step 1.
;;    For example,
;;
;;         (pg-connect "dbname" "user" :port 3333)
;;
;;    You can omit the port argument if you chose 5432 as the local
;;    end of the tunnel, since pg.lisp defaults to this value.
;;
;;
;; This code has been tested with
;;
;;   * CMUCL 18b and 18c on SPARC
;;   * CLISP 2000-03-06 on LinuxPPC
;;   * ACL503 trial/x86
;;   * PostgreSQL 6.3 and 7.0
;;
;; Please note that your postmaster has to be started with the `-i'
;; option in order for i to accept TCP/IP connections (typically this
;; is not the default setting).
;;
;; Thanks to Marc Battyani for the LW port and for bugfixes.


;;; TODO ============================================================
;;
;; * add a mechanism for parsing user-defined types. The user should
;;   be able to define a parse function and a type-name; we query
;;   pg_type to get the type's OID and add the information to
;;   pg:*parsers*.


(defpackage :postgresql
  (:nicknames :pg)
  (:use :common-lisp
        #+cmu :alien
        #+cmu :c-call)
  (:export #:pg-connect #:pg-exec #:pg-result #:pg-disconnect
           #:pg-disable-type-coercion
           #:pg-databases #:pg-tables #:pg-columns
           #:with-pg-connection))
(in-package :pg)

#+allegro (require :socket)
#+lispworks (require "comm")
#+corman (require :sockets)


(define-condition postgresql-error (simple-error))
(define-condition connection-failure (postgresql-error)
  ((host :initarg :host
         :reader connection-failure-host)
   (port :initarg :port
         :reader connection-failure-port))                                          
  (:report
   (lambda (exc stream)
     (format stream "Couldn't connect to PostgreSQL database at ~a:~a.
Is the postmaster running and accepting TCP connections?~%"
             (connection-failure-host exc)
             (connection-failure-port exc)))))
             

(defconstant +NAMEDATALEN+ 32)          ; postgres_ext.h
(defconstant +PG_PROTOCOL_LATEST_MAJOR+ 1) ; libpq/pgcomm.h
(defconstant +PG_PROTOCOL_LATEST_MINOR+ 0)
(defconstant +SM_DATABASE+ 64)
(defconstant +SM_USER+     32)
(defconstant +SM_OPTIONS+  64)
(defconstant +SM_UNUSED+   64)
(defconstant +SM_TTY+      64)

(defconstant +AUTH_REQ_OK+       0)
(defconstant +AUTH_REQ_KRB4+     1)
(defconstant +AUTH_REQ_KRB5+     2)
(defconstant +AUTH_REQ_PASSWORD+ 3)
(defconstant +AUTH_REQ_CRYPT+    4)

(defconstant +STARTUP_MSG+            7)
(defconstant +STARTUP_KRB4_MSG+      10)
(defconstant +STARTUP_KRB5_MSG+      11)
(defconstant +STARTUP_PASSWORD_MSG+  14)

(defconstant +STARTUP_PACKET_SIZE+
  (+ 4 4 +SM_DATABASE+ +SM_USER+ +SM_OPTIONS+ +SM_UNUSED+ +SM_TTY+))

(defconstant +MAX_MESSAGE_LEN+    8192)     ; libpq-fe.h


(defstruct pgcon stream (binary-p nil))
(defstruct pgresult connection status attributes tuples)


(defmacro with-pg-connection ((con &rest open-args) &body (forms decls))
  "Bindspec is of the form (connection open-args), where OPEN-ARGS are
as for PG-CONNECT. The database connection is bound to the variable
CONNECTION. If the connection is unsuccessful, the forms are not
evaluated. Otherwise, the forms are executed, and upon termination,
normal or otherwise, the database connection is closed."
  `(let ((,con (pg-connect ,@open-args)))
     ,@decls
     (unwind-protect
         (progn ,@forms)
       (when ,con (pg-disconnect ,con)))))

(defun pg-connect (dbname user &key (host "localhost") (port 5432) (password ""))
  "Initiate a connection with the PostgreSQL backend.
Connect to the database DBNAME with the username USER,
on PORT of HOST, providing PASSWORD if necessary. Return a
connection to the database (as an opaque type)."
  (let* ((stream (socket-connect port host))
         (connection (make-pgcon :stream stream))
         (user-packet-length (+ +SM_USER+ +SM_OPTIONS+ +SM_UNUSED+ +SM_TTY+)))
    ;; send the startup packet
    (send-int connection +STARTUP_PACKET_SIZE+ 4)
    (send-int connection +PG_PROTOCOL_LATEST_MAJOR+ 2)
    (send-int connection +PG_PROTOCOL_LATEST_MINOR+ 2)
    (send-string connection dbname +SM_DATABASE+)
    (send-string connection user user-packet-length)
    (flush connection)
    #+cmu
    (ext:finalize connection
                  (lambda () (pg-disconnect connection)))
    (loop
     (case (read-byte stream)
       ((69) (error (read-cstring connection 4096)))
       ((82)
        (let ((areq (read-net-int connection 4)))
          (cond
           ((= areq +AUTH_REQ_OK+)
            (and (not pg-disable-type-coercion)
                 (null *parsers*)
                 (initialize-parsers connection))
            (pg-exec connection "SET datestyle = 'ISO'")
            (return connection))
           ((= areq +AUTH_REQ_PASSWORD+)
            (send-int connection (+ 5 (length password)) 4)
            (send-string connection password)
            (send-int connection 0 1)
            (flush connection))
           ((= areq +AUTH_REQ_CRYPT+)
            ;;: this isn't working for some reason, even with psql.
            (let* ((salt (read-chars connection 2))
                   (crypted (crypt password salt)))
              (format *debug-io* "Got salt of ~s~%" salt)
              (send-int connection (+ 5 (length crypted)) 4)
              (send-string connection crypted)
              (send-int connection 0 1)
              (flush connection)))
           ((= areq +AUTH_REQ_KRB4+)
            (error "Kerberos4 authentication not supported"))
           ((= areq +AUTH_REQ_KRB5+)
            (error "Kerberos5 authentication not supported"))
           (t
            (error "Can't do that type of authentication" areq)))))
       (t
        (error "Problem connecting: expected an authentication response"))))))

(defun pg-exec (connection &rest args)
  "Execute the SQL command given by the concatenation of ARGS
on the database to which we are connected via CONNECTION. Return
a result structure which can be decoded using `pg-result'."
  (let ((sql (apply #'concatenate 'string args))
        (stream (pgcon-stream connection))
        (tuples '())
        (attributes '())
        (result (make-pgresult :connection connection)))
    (when (> (length sql) +MAX_MESSAGE_LEN+)
      (error "SQL statement too long" sql))
    (write-byte 81 stream)
    (send-string connection sql)
    (write-byte 0 stream)
    (flush connection)
    (do ((b (read-byte stream nil :eof)
            (read-byte stream nil :eof)))
        ((eq b :eof) (error "EOF from backend"))
      (case b
        ((65)                           ; asynchronous notify
         (let ((pid (read-net-int connection 4))
               (msg (read-cstring connection +MAX_MESSAGE_LEN+)))
           (declare (ignore pid))
           (format *debug-io* "Asynchronous notify ~a" msg)))

        ((66)                           ; binary data transfer
         (setf (pgcon-binary-p connection) t)
         (or attributes (error "Tuple received before metadata"))
         (push (read-tuple connection attributes) tuples))

        ((67)                           ; command status
         (let ((status (read-cstring connection +MAX_MESSAGE_LEN+)))
           (setf (pgresult-status result) status)
           (setf (pgresult-tuples result) (nreverse tuples))
           (setf (pgresult-attributes result) attributes)
           (return result)))

        ((68)                           ; text data transfer
         (setf (pgcon-binary-p connection) nil)
         (unless attributes (error "Tuple received before metadata"))
         (push (read-tuple connection attributes) tuples))

        ((69)                           ; error message
         (let ((msg (read-cstring connection +MAX_MESSAGE_LEN+)))
           (error "Backend error ~s" msg)))

        ;; indicates the end of a series of command statuses, for example
        ((73)                           ; empty query
         (let ((c (read-char connection)))
           (if (char< #\null c)
               (error "Garbled data"))))

        ((78)                           ; error notification
         (let ((notice (read-cstring connection +MAX_MESSAGE_LEN+)))
           (format *error-output* "NOTICE: ~a~%" notice)))

        ((80)                           ; synchronous portal
         (let ((str (read-cstring connection +MAX_MESSAGE_LEN+)))
           (declare (ignore str))
           ;; (format *debug-io* "Portal name ~a~%" str)
           ))

        ((84)                           ; metadata field description
         (and attributes (error "Cannot handle multiple result group"))
         (setq attributes (read-attributes connection)))
        
        (t
         (error "Unknown response type from backend ~d" b))))))
  

(defun pg-result (result what &rest args)
  "Extract WHAT component of RESULT.
RESULT should be a structure obtained from a call to `pg-exec',
and WHAT should be one of
   :connection -> return the connection object
   :status -> return the status string provided by the database
   :attributes -> return the metadata, as a list of lists
   :tuples -> return the data, as a list of lists
   :tuple n -> return the nth component of the data
   :oid -> return the OID (a unique identifier generated by PostgreSQL
           for each row resulting from an insertion"
  (cond ((eq :connection what) (pgresult-connection result))
        ((eq :status what)     (pgresult-status result))
        ((eq :attributes what) (pgresult-attributes result))
        ((eq :tuples what)     (pgresult-tuples result))
        ((eq :tuple what)
         (let ((which (if args (first args) (error "which tuple?")))
               (tuples (pgresult-tuples result)))
           (nth which tuples)))
        ((eq :oid what)
         (let ((status (pgresult-status result)))
           (if (string= "INSERT" (subseq status 0 6))
               (parse-integer (subseq status 7 (position #\space status :start 7)))
               (error "Only INSERT commands generate an oid: ~s" status))))
        (t (error "Unknown result request: ~s" what))))

(defun pg-disconnect (connection)
  (write-byte 88 (pgcon-stream connection))
  (flush connection)
  (close (pgcon-stream connection))
  (values))



;; Attribute information is as follows
;;    attribute-name (string)
;;    attribute-type as an oid from table pg_type
;;    attribute-size (in bytes?)
(defun read-attributes (connection)
  (let ((attribute-count (read-net-int connection 2))
        (attributes '()))
    (do ((i attribute-count (- i 1)))
        ((zerop i) (nreverse attributes))
      (let ((type-name (read-cstring connection +MAX_MESSAGE_LEN+))
            (type-id   (read-net-int connection 4))
            (type-len  (read-net-int connection 2)))
        (push (list type-name type-id type-len) attributes)))))

;; a bitmap is a string, which we interpret as a sequence of bytes
(defun bitmap-ref (bitmap ref)
  (multiple-value-bind (char-ref bit-ref)
      (floor ref 8)
    (logand #b10000000 (ash (aref bitmap char-ref) bit-ref))))
    
;; the server starts by sending a bitmap indicating which tuples are
;; NULL
(defun read-tuple (connection attributes)
  (let* ((num-attributes (length attributes))
         (num-bytes (ceiling (/ num-attributes 8)))
         (bitmap (read-bytes connection num-bytes))
         (correction (if (pgcon-binary-p connection) 0 -4))
         (tuples '()))
    (do ((i 0 (+ i 1))
         (type-ids (mapcar #'second attributes) (cdr type-ids)))
        ((= i num-attributes) (nreverse tuples))
      (cond ((zerop (bitmap-ref bitmap i))
             (push nil tuples))
            (t
             (let* ((len (+ (read-net-int connection 4) correction))
                    (raw (read-chars connection (max 0 len)))
                    (parsed (parse raw (car type-ids))))
               (push parsed tuples)))))))


;; type coercion support ==============================================
;; 
;; When returning data from a SELECT statement, PostgreSQL starts by
;; sending some metadata describing the attributes. This information
;; is read by `PG:READ-ATTRIBUTES', and consists of each attribute's
;; name (as a string), its size (in bytes), and its type (as an oid
;; which points to a row in the PostgreSQL system table pg_type). Each
;; row in pg_type includes the type's name (as a string).
;;
;; We are able to parse a certain number of the PostgreSQL types (for
;; example, numeric data is converted to a numeric Common Lisp type,
;; dates are converted to the CL date representation, booleans to
;; lisp booleans). However, there isn't a fixed mapping from a
;; type to its OID which is guaranteed to be stable across database
;; installations, so we need to build a table mapping OIDs to parser
;; functions.
;;
;; This is done by the procedure `PG:INITIALIZE-PARSERS', which is run
;; the first time a connection is initiated with the database from
;; this invocation of CL, and which issues a SELECT statement to
;; extract the required information from pg_type. This initialization
;; imposes a slight overhead on the first request, which you can avoid
;; by setting `PG-DISABLE-TYPE-COERCION' to non-nil if it bothers you.
;; ====================================================================

(defvar pg-disable-type-coercion nil
  "Non-nil disables the type coercion mechanism.
The default is nil, which means that data recovered from the
database is coerced to the corresponding Common Lisp type before
being returned; for example numeric data is transformed to CL
numbers, and booleans to booleans.

The coercion mechanism requires an initialization query to the
database, in order to build a table mapping type names to OIDs. This
option is provided mainly in case you wish to avoid the overhead of
this initial query. The overhead is only incurred once per session
(not per connection to the backend).")


;; see `man pgbuiltin' for details on PostgreSQL builtin types
(defun number-parser (str) (parse-integer str))
(defun text-parser (str) str)
(defun bool-parser (str)
  (cond ((string= "t" str) t)
        ((string= "f" str) nil)
        (t (error "Badly formed boolean from backend: ~s" str))))

;; format for abstime/timestamp etc with ISO output syntax is
;;;    "1999-01-02 00:00:00+01"
;; which we convert to a CL universal time
(defun timestamp-parser (str)
  (let ((year    (parse-integer (subseq str 0 4)))
        (month   (parse-integer (subseq str 5 7)))
        (day     (parse-integer (subseq str 8 10)))
        (hours   (parse-integer (subseq str 11 13)))
        (minutes (parse-integer (subseq str 14 16)))
        (seconds (parse-integer (subseq str 17 19)))
        (tz      (parse-integer (subseq str 19 22))))
    (declare (ignore tz))
    (encode-universal-time seconds minutes hours day month year)))

;; format for abstime/timestamp etc with ISO output syntax is
;;;    "1999-01-02 00:00:00+01"
;; which we convert to a CL universal time
(defun isodate-parser (str)
  (let ((year    (parse-integer (subseq str 0 4)))
        (month   (parse-integer (subseq str 5 7)))
        (day     (parse-integer (subseq str 8 10)))
        (hours   (parse-integer (subseq str 11 13)))
        (minutes (parse-integer (subseq str 14 16)))
        (seconds (parse-integer (subseq str 17 19)))
        (tz      (parse-integer (subseq str 19 22))))
    (declare (ignore tz))
    (encode-universal-time seconds minutes hours day month year tz)))

(defun initialize-parsers (connection)
  (let* ((pgtypes (pg-exec connection "SELECT typname,oid FROM pg_type"))
         (tuples (pg-result pgtypes :tuples)))
    (setq *parsers* '())
    (map nil
     (lambda (tuple)
       (let* ((typname (first tuple))
              (oid (parse-integer (second tuple)))
              (type (assoc typname type-parsers :test #'string=)))
         (if (consp type)
             (push (cons oid (cdr type)) *parsers*))))
     tuples)))

(defun parse (str oid)
  (let ((parser (assoc oid *parsers* :test #'eq)))
    (if (consp parser)
        (funcall (cdr parser) str)
        str)))

;; alist of (oid . parser) pairs. This is built dynamically at
;; initialization of the connection with the database (once generated,
;; the information is shared between connections).
(defvar *parsers* '())

(defvar type-parsers
  `(("bool"      . ,'bool-parser)
    ("char"      . ,'text-parser)
    ("char2"     . ,'text-parser)
    ("char4"     . ,'text-parser)
    ("char8"     . ,'text-parser)
    ("char16"    . ,'text-parser)
    ("text"      . ,'text-parser)
    ("varchar"   . ,'text-parser)
    ("int2"      . ,'number-parser)
    ("int28"     . ,'number-parser)
    ("int4"      . ,'number-parser)
    ("oid"       . ,'number-parser)
    ("float4"    . ,'number-parser)
    ("float8"    . ,'number-parser)
    ("money"     . ,'number-parser)
    ("abstime"   . ,'timestamp-parser)
    ("date"      . ,'timestamp-parser)  ; this is probably incorrect: only "2000-12-09"
    ("timestamp" . ,'timestamp-parser)
    ("datetime"  . ,'timestamp-parser)
    ("time"      . ,'text-parser)     ; preparsed "15:32:45"
    ("reltime"   . ,'text-parser)     ; don't know how to parse these
    ("timespan"  . ,'text-parser)
    ("tinterval" . ,'text-parser)))


;; large objects support ===============================================
;;
;; Sir Humphrey: Who is Large and to what does he object?
;;
;; Large objects are the PostgreSQL way of doing what most databases
;; call BLOBs (binary large objects). In addition to being able to
;; stream data to and from large objects, PostgreSQL's
;; object-relational capabilities allow the user to provide functions
;; which act on the objects.
;;
;; For example, the user can define a new type called "circle", and
;; define a C or Tcl function called `circumference' which will act on
;; circles. There is also an inheritance mechanism in PostgreSQL. 
;; =====================================================================

;; (defvar *lo-initialized* nil)
;; (defvar *lo-functions* '())
;; 
;; (defun lo-init (connection)
;;   (let ((res (pg-exec connection
;;                     "SELECT proname, oid from pg_proc WHERE "
;;                     "proname = 'lo_open' OR "
;;                     "proname = 'lo_close' OR "
;;                     "proname = 'lo_creat' OR "
;;                     "proname = 'lo_unlink' OR "
;;                     "proname = 'lo_lseek' OR "
;;                     "proname = 'lo_tell' OR "
;;                     "proname = 'loread' OR "
;;                     "proname = 'lowrite'")))
;;     (setq *lo-functions* '())
;;     (dolist (tuple (pg-result res :tuples))
;;       (push (cons (car tuple) (cadr tuple)) *lo-functions*))
;;     (setq *lo-initialized* t)))
;; 
;; ;; fn is either an integer, in which case it is the OID of an element
;; ;; in the pg_proc table, and otherwise it is a string which we look up
;; ;; in the alist *lo-functions* to find the corresponding OID.
;; (defun fn (connection fn integer-result &rest args)
;;   (or *lo-initialized* (lo-init connection))
;;   (let ((fnid (cond ((integerp fn) fn)
;;                     ((not (stringp fn))
;;                      (error "Expecting a string or an integer: ~s" fn))
;;                     ((assoc fn *lo-functions*)
;;                      (cdr (assoc fn *lo-functions*)))
;;                     (t
;;                      (error "Unknown builtin function ~s" fn)))))
;;     (send-byte connection 70)
;;     (send-byte connection 0)
;;     (send-int connection fnid 4)
;;     (send-int connection (length args) 4)
;;     (loop :for arg in args
;;           :when (integerp arg) do
;;             (send-int connection 4 4)
;;             (send-int connection arg 4)
;;           :when (stringp arg) do
;;             (send-int connection (string-length arg) 4)
;;             (send-string connection arg))
;;           :else do
;;             (error "Unknown fastpath type ~s" arg))))
;;     (flush connection)
;;     ;; we should receive #\V on success or #\E on error
;; ;;     (let ((c (read-char connection)))
;; ;;       (case c
;; ;;         ((#\E) (error (read-cstring connection 4096)))
;; ;;         (else
;; ;;          (error "Unexpected character in fn" c))))
;;     (let loop ((result '()))
;;       (let ((c (read-char connection)))
;;         (case c
;;           ((#\E) (error (read-cstring connection 4096)))
;;           ((#\G)                        ; function returned OK
;;            (let* ((len (read-net-int connection 4))
;;                   (res (if integer-result
;;                            (read-net-int connection len)
;;                            (read-chars connection len))))
;;              (loop res)))
;;           ((#\N)
;;            (let ((notice (read-cstring connection MAX_MESSAGE_LEN)))
;;              (format (error-output-port) "NOTICE: ~a~%" notice))
;;            (force-output)
;;            (loop result))
;;           ((#\0)
;;            result)
;;           ((#\V)                        ; bogus!
;;            (loop #t))
;;           (else
;;            (error "Unexpected character in fn" c)))))))
;; 
;;     
;; ;; returns an OID
;; (defun lo-create (connection &rest args)
;;   (let-optionals* args
;;       ((modestr "r")
;;        (mode (cond ((string-ci=? "r" modestr) INV_READ)
;;                    ((string-ci=? "w" modestr) INV_WRITE)
;;                    ((string-ci=? "rw" modestr)
;;                     (bitwise-ior INV_READ INV_WRITE))
;;                    ((integer? modestr) modestr)
;;                    (else (error "Bad mode" modestr))))
;;        (oid (fn connection "lo_creat" #t mode)))
;;     (cond ((not (integer? oid))
;;            (error "Didn't return an OID" oid))
;;           ((zero? oid)
;;            (error "Can't create large object"))
;;           (else oid))))
;; 
;; ;; args = modestring (default "r", or "w" or "rw")
;; ;; returns a file descriptor for use in later lo-* procedures        
;; (defun lo-open (connection oid &rest args)
;;   (let-optionals* args
;;      ((modestr "r")
;;       (mode (cond ((string-ci=? "r" modestr) INV_READ)
;;                   ((string-ci=? "w" modestr) INV_WRITE)
;;                   ((string-ci=? "rw" modestr)
;;                    (bitwise-ior INV_READ INV_WRITE))
;;                   ((integer? modestr) modestr)
;;                   (else (error "Bad mode" modestr)))))
;;      (fn connection "lo_open" #t oid mode)))
;; 
;; (defun lo-close (connection fd)
;;   (fn connection "lo_close" #t fd))
;; 
;; (defun lo-read (connection fd bytes)
;;   (fn connection "loread" #f fd bytes))
;; 
;; (defun lo-write (connection fd buf)
;;   (fn connection "lowrite" #t fd buf))
;;   
;; (defun lo-lseek (connection fd offset whence)
;;   (fn connection "lo_lseek" #t fd offset whence))
;; 
;; (defun lo-tell (connection oid)
;;   (fn connection "lo_tell" #t oid))
;;   
;; (defun lo-unlink (connection oid)
;;   (fn connection "lo_unlink" #t oid))
;; 
;; (defun lo-import (connection filename)
;;   (let* ((fdin (open-input-file filename))
;;          (oid (lo-create connection "rw"))
;;          (fdout (lo-open connection oid "w")))
;;     (do ((str (read-cstring 1024 fdin)
;;               (read-cstring 1024 fdin)))
;;         ((not str))
;;       (lo-write connection fdout str))
;;     (close fdin)
;;     (lo-close connection fdout)
;;     oid))         
;; 
;; (defun lo-export (connection oid filename)
;;   (let* ((fdout (open-output-file filename))
;;          (fdin (lo-open connection oid "r")))
;;     (do ((str (lo-read connection fdin 1024)
;;               (lo-read connection fdin 1024)))
;;         ((or (not str)
;;              (zero? (string-length str))
;;              (eof-object? str)))
;;       (format t "Read ~s in lo-export~%" str)
;;       (write-string str fdout))
;;     (lo-close connection fdin)
;;     (close fdout)))



;; DBMS metainformation ================================================
;;
;; Metainformation such as the list of databases present in the
;; database management system, list of tables, attributes per table.
;; This information is not available directly, but can be deduced by
;; querying the system tables.
;;
;; Based on the queries issued by psql in response to user commands
;; `\d' and `\d tablename'; see file pgsql/src/bin/psql/psql.c
;; =====================================================================
(defun pg-databases (conn)
  "Return a list of the databases available at this site."
  (let ((res (pg-exec conn "SELECT datname FROM pg_database")))
    (apply #'append (pg-result res :tuples))))

(defun pg-tables (conn)
  "Return a list of the tables present in this database."
  (let ((res (pg-exec conn "SELECT relname FROM pg_class, pg_user WHERE "
                      "(relkind = 'r' OR relkind = 'i' OR relkind = 'S') AND "
                      "relname !~ '^pg_' AND usesysid = relowner ORDER BY relname")))
    (apply #'append (pg-result res :tuples))))
    
(defun pg-columns (conn table)
  "Return a list of the columns present in TABLE."
  (let* ((sql (format "SELECT * FROM ~s WHERE 0 = 1" table))
         (res (pg-exec conn sql)))
    (mapcar #'first (pg-result res :attributes))))


;; support routines ===================================================

;; read an integer in network byte order
(defun read-net-int (connection bytes)
  (do ((i bytes (- i 1))
       (stream (pgcon-stream connection))
       (accum 0))
      ((zerop i) accum)
    (setq accum (+ (* 256 accum) (read-byte stream)))))

(defun read-int (connection bytes)
  (do ((i bytes (- i 1))
       (stream (pgcon-stream connection))
       (multiplier 1 (* multiplier 256))
       (accum 0))
      ((zerop i) accum)
    (incf accum (* multiplier (read-byte stream)))))

(defun read-bytes (connection howmany)
  (let ((v (make-array howmany :element-type 'unsigned-byte))
        (s (pgcon-stream connection)))
    (read-sequence v s)
    v))

(defun read-chars (connection howmany)
  (let ((bytes (read-bytes connection howmany))
        (str (make-string howmany)))
    (dotimes (i howmany)
      (setf (aref str i) (code-char (aref bytes i))))
    str))

  
(defun read-cstring (connection maxbytes)      
  "Read a null-terminated string from CONNECTION."
  (let ((stream (pgcon-stream connection))
        (chars nil))
    (do ((b (read-byte stream nil nil) (read-byte stream nil nil))
         (i 0 (+ i 1)))
        ((or (= i maxbytes)             ; reached allowed length
             (null b)                   ; eof
             (zerop b))                 ; end of string
         (concatenate 'string (nreverse chars)))
      (push (code-char b) chars))))

;; highest order bits first
(defun send-int (connection int bytes)
  (let ((v (make-array bytes :element-type 'unsigned-byte))
        (stream (pgcon-stream connection)))
    (do ((i (- bytes 1) (- i 1)))
        ((< i 0))
      (setf (aref v i) (rem int 256))
      (setq int (floor int 256)))
    (write-sequence v stream)))          
    
(defun send-string (connection str &optional pad-to)
  (let* ((stream (pgcon-stream connection))
         (len (length str))
         (v (make-array len :element-type 'unsigned-byte)))
    ;; convert the string to a vector of bytes
    (dotimes (i len)
      (setf (aref v i) (char-code (aref str i))))
    (write-sequence v stream)
    ;; pad if necessary
    (when pad-to
      (let* ((padding-length (- pad-to len))
             (padding (make-array padding-length :initial-element 0)))
        (write-sequence padding stream)))))

(defun flush (connection)
  (force-output (pgcon-stream connection)))

;; ;; double any ' characters. What else needs quoting?
;; (defun (quote connection obj)
;;   (cond ((number? obj) (number->string obj))
;;         ((char? obj) (string #\' obj #\'))
;;         ((eq? t obj) "'t'")
;;         ((eq? nil obj) "'f'")
;;         ((string? obj) (string-append "'" (string-replace "'" "''" obj) "'"))
;;         (t (error "Don't know how to quote that" obj))))
;; 
;; ;; replace all occurrences of OLD by NEW in the string STR
;; (defun (string-replace old new str)
;;   ;; I'm lazy
;;   (regexp-substitute/global nil (regexp-quote old) str 'pre new 'post))


;; #+CLISP
;; (defun crypt (key salt)
;;   (linux::crypt key salt))

#+cmu
(alien:def-alien-routine ("crypt" unix-crypt) c-string
  (key c-string)
  (salt c-string))

(defun crypt (key salt)
  (declare (ignore salt))
  key)

#+cmu
(defun socket-connect (port host)
  (handler-case 
   (let ((fd (ext:connect-to-inet-socket host port)))
     (sys:make-fd-stream fd :input t :output t
                         :element-type '(unsigned-byte 8)))
   (error (e)
      (declare (ignore e))
      (signal 'connection-failure :host host :port port))))
  
#+clisp
(defun socket-connect (port host)
  (handler-case
   (lisp:socket-connect port host :element-type '(unsigned-byte 8))
   (error (e)
      (declare (ignore e))
      (signal 'connection-failure :host host :port port))))


#+db-sockets
(defun socket-connect (port host)
  (handler-case
   (let ((s (sockets:make-inet-socket :stream :tcp))
         (num (car (sockets:host-ent-addresses
                    (sockets:get-host-by-name host)))))
     (sockets:socket-connect s num port)
     (sockets:socket-make-stream s :element-type '(unsigned-byte 8)
                                 :input t :output t :buffering :none))
   (error (e)
      (declare (ignore e))
      (signal 'connection-failure :host host :port port))))

#+allegro
(defun socket-connect (port host)
  (handler-case
   (socket:make-socket :remote-host host
                       :remote-port port
                       :format :binary)
   (error (e)
      (declare (ignore e))
      (signal 'connection-failure :host host :port port))))

;; There is a bug in Lispworks regarding binary I/O on socket streams.
#+lispworks
(defun socket-connect (port host)
  (comm:open-tcp-stream host port
			:element-type 'base-char ;'unsigned-byte
			:direction :io))

#+corman
(defun socket-connect (port host)
  (let ((sock (make-client-socket :host host :port port)))
    (sockets:make-socket-stream sock)))



;; == testing ==============================================================
(defun test-general ()
  (let* ((conn (pg-connect "test" "sysadmin" :password "sysadmin" :host "locke"))
         (r1 (pg-exec conn "SELECT * FROM test"))
         (r2 (pg-exec conn "CREATE TABLE pgltest (a int)"))
         (r3 (pg-exec conn "INSERT INTO pgltest VALUES (3)"))
         (r4 (pg-exec conn "DROP TABLE pgltest")))
    (format t "~%==============================================~%")
    (format t "status of SELECT statement is ~s~%" (pg-result r1 :status))
    (format t "attributes of SELECT statement are ~s~%"
            (pg-result r1 :attributes))
    (format t "tuples of SELECT statement are ~s~%" (pg-result r1 :tuples))
    (format t "second tuple of SELECT is ~s~%" (pg-result r1 :tuple 1))
    (format t "status of CREATE is ~s~%" (pg-result r2 :status))
    (format t "status of INSERT is ~s~%" (pg-result r3 :status))
    (format t "oid of INSERT is ~s~%" (pg-result r3 :oid))
    (format t "status of DROP is ~s~%" (pg-result r4 :status))
    (format t "==============================================~%")
    (pg-disconnect conn)))

(defun test-date ()
  (with-pg-connection (conn "test" "sysadmin" :password "sysadmin" :host "locke")
     (pg-exec conn "DROP TABLE pgltest")
     (pg-exec conn "CREATE TABLE pgltest (a timestamp, b abstime, c time)")
     (pg-exec conn "INSERT INTO pgltest VALUES "
              "(current_timestamp, 'now', 'now')")
     (let* ((res (pg-exec conn "SELECT * FROM pgltest"))
            (parsed (first (pg-result res :tuples))))
       (format t "Timestamp = ~s~%abstime = ~s~%time = ~s~%"
               (first parsed)
               (second parsed)
               (third parsed)))
     (pg-exec conn "DROP TABLE pgltest")))

;; EOF
