;;; shakespeare-quote.el - Insert Shakespeare quote into outgoing news.
;;; Copyright (C) 2001 Tom Fawcett (fawcett -at- croftj -dot- net)
 
;;; This file is not part of GNU Emacs or XEmacs.  Not even close.

;;; This program is free software; you may redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to fawcett -at- croftj -dot- net)
;;; or from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; $Revision: 1.1.1.1 $

;;; Commentary:

;;;   This package inserts a random Shakespeare quote into an outgoing news
;;;   message, using a header field like X-Shakespeare.
;;;
;;;   This package has been tested with Xemacs 21.1.4.
;;;
;;;   The author is not adept at elisp programming.
;;;   Bug fixes, suggestions and new Shakespeare quotes are welcome.
;;;
;;; Installation:
;;;
;;;   1. Put `shakespeare-quote.el' somewhere in your Emacs Lisp load path.
;;;
;;;   2. Add the following to your `.emacs' file (or elsewhere):
;;;
;;;          (autoload 'shq-insert-quote "shakespeare-quote" "" t)
;;;          
;;;   3. Depending on how often you post and how much Shakespeare you can
;;;      stand, you may also want to add this to your .gnus file:
;;;
;;;          (add-hook 'gnus-message-setup-hook 'shq-insert-quote)
;;;      
;;;
;;; ------
;;; From "News Article Format and Transmission":
;;;   Posters wishing to convey non-standard information in headers SHOULD use
;;;   header names beginning with "X-".  No standard header name will ever be
;;;   of this form.  Reading agents SHOULD ignore "X-" headers, or at least
;;;   treat them with great care.
;;;

(require 'message)

(defvar shq-field "X-Shakespeare"
  "The header name under which the Shakespeare quote will appear.
This should start with 'X-'.")

(defvar shq-indentation 
  (let ((number-of-spaces (+ (length shq-field) 2)))
    (make-string number-of-spaces ? ))
  "The amount of indentation to use for formatting Shakespeare quotes.
This is based on the contents of shq-field plus ': '")

(defvar shq-after-these-headers
  (mapcar #'(lambda (x) (prin1-to-string (if (consp x) (cdr x) x)))
	  message-required-news-headers)
  "*Headers after which to insert a Shakespeare quote.
NIL => insert it first (this is not encouraged).
Default is all required headers specified by message-required-news-headers")

(defvar shq-quote-limit nil
  "Maximum number of lines in a Shakespeare quote, or NIL.
Quotes having greater than this number of lines will not be chosen.
NIL => no limit imposed.")


;;;###autoload
(defun shq-insert-quote nil
  "Insert a Shakespeare quote into the current outgoing news or mail message.
Uses the header specified by shq-field, usually something like
'X-Shakespeare'.   Existing contents of this header are deleted first,
so calling this function multiple times will keep changing the quote."
  (interactive)
  (let* ((quot (shq-random-shakespeare-quote))
	 (lines (butlast quot))
	 (source (car (last quot)))
	 (play (first source))
	 (play-lines (second source))
	 (comment (third source)))

    (let ((lines-formatted (concat 
			    "\"" 
			    (mapconcat #'identity lines
				       (concat "\n" shq-indentation))
			    "\""
			    ))
	  (attribution-line (shq-attribution-line play play-lines comment))
	  )
      ;;  Remove existing header and insert new one
      (message-remove-header shq-field)
      (apply #'message-position-on-field
	     shq-field
	     shq-after-these-headers)
      ;;  Insert the quote
      (insert lines-formatted)
      ;;  Insert attribution text if it fits on last line, else put on a new line
      (if (> (+ (current-column) (length attribution-line) 5) 80)
	  (insert "\n" shq-indentation "     " attribution-line)
	(insert " " attribution-line))
      )
    ))

(defun shq-random-shakespeare-quote ()
  "Retrieves a random Shakespeare quote, respecting shq-quote-limit"
  (let (quote quote-lines)
    (while (or (null quote) 
	       (and (numberp shq-quote-limit)
		    (> quote-lines shq-quote-limit)))
      (setq quote (aref shakespeare-quotes (random (length shakespeare-quotes)))
	    quote-lines (1- (length quote))))
    quote))
      

(defun shq-attribution-line (play play-lines comment)
  "Returns the formatted attribution line of a Shakespeare quote, as a string.
PLAY-LINES and COMMENT are optional."
  (concat
   (if play (format "-- %s" play) "")
   (if play-lines (format ", %s" play-lines) "")
   (if comment (format " (%s)" comment))
   ))


;; my attempt to create a new command. Lost patience with it.
(defun shq-insert-quote ()
 "Insert a random Shakespeare quote into the current buffer."
 (interactive)
 (insert 'shq-random-shakespeare-quote)
)


;;  Big bag o' Shakespeare quotes.
;;  Format is array of (LINE1 LINE2 LINE3 ... SOURCE)
;;  Each LINEi is a string which will be formatted on a separate line.
;;  SOURCE is a list of (PLAY WHERE COMMENT).  
;;  PLAY is the name of the play (or sonnet).
;;  WHERE and COMMENT are optional.
;;  If present, WHERE is ACT.SCENE or ACT.SCENE.LINES.
;;  ACT is customarily a roman numeral, SCENE is arabic numeral.
;;  COMMENT is a string that will be placed in parentheses at the very end.
(defconst shakespeare-quotes
  [

   ("Courage mounteth with occasion"
    ("King John" "II.1.82"))
     
   ("At Christmas I no more desire a rose"
    "Than wish a snow in May's new-fangled shows;"
    "But like of each thing that in season grows."
    ("Love's Labour's Lost" "I.1.105-7" "Biron to Ferdinand, King of Navarre"))

   ("If it were done when 'tis done, then 'twere well"
    "It were done quickly."
    ("Macbeth" "I.7"))

   ("I would there were no age between sixteen and"
    "three-and-twenty, or that youth would sleep out the"
    "rest; for there is nothing in the between but"
    "getting wenches with child, wronging the ancientry,"
    "stealing, fighting"
    ("The Winter's Tale" "III.3"))

   ("The dread of something after death,"
    "The undiscovered country, from whose bourn"
    "No traveller returns."
    ("Hamlet" "III.1"))

   ("MacDuff: What three things does drink especially provoke?"
    "Porter: Marry, sir, nose-painting, sleep, and"
    "urine.  Lechery, sir, it provokes, and unprovokes;"
    "it provokes the desire, but it takes"
    "away the performance."
    ("Macbeth" "II.3"))

   ("You are not worth the dust"
    "which the rude wind blows in your face."
    ("King Lear" "IV.2"))

   ("Reputation is an idle and most false imposition,"
    "oft got without merit and lost without deserving."
    ("Othello" "II.3"))

   ("The devil can cite Scripture for his purpose."
    ("The Merchant of Venice" "I.3"))

   ("O, let my books be then the eloquence"
    "And dumb presagers of my speaking breast."
    ("Sonnet 23"))

   ("Your old virginity is like one of our French withered pears:"
    "it looks ill, it eats drily."
    ("All's Well That Ends Well" "I.1"))

   ("Nothing can seem foul to those that win."
    ("King Henry IV, pt. 1" "V.1"))

   ("But screw your courage to the sticking-place"
    "And we'll not fail."
    ("Macbeth" "I.7"))

   ("Life's but a walking shadow, a poor player"
    "That struts and frets his hour upon the stage"
    "And then is heard no more: it is a tale"
    "Told by an idiot, full of sound and fury,"
    "Signifying nothing."
    ("Macbeth" "V.5"))

   ("To-morrow, and to-morrow, and to-morrow,"
    "creeps in this petty pace from day to day"
    ("Macbeth" "V.5"))

   ("Had I but served my God with half the zeal"
    "I had served my king, he would not in mine age"
    "Have left me naked to mine enemies."
    ("King Henry VIII" "III.2" "Cardinal Wolsey to Cromwell"))

   ("I had rather have a fool to make me merry"
    "than experience to make me sad."
    ("As You Like It" "IV.1"))

   ("I could be bounded in a nut shell and count"
    "myself a king of infinite space, were it not that I"
    "have bad dreams."
    ("Hamlet" "II.2" "Hamlet to Rosencrantz and Guildenstern"))

   ("The strongest oaths are straw to the fire in the blood."
    ("The Tempest" "III.3"))

   ("Love is not love"
    "Which alters when it alteration finds,"
    "Or bends with the remover to remove."
    ("Sonnet 141"))

   ("All the world's a stage,"
    "And all the men and women merely players:"
    "They have their exits and their entrances;"
    "And one man in his time plays many parts"
    ("As You Like It" "II.7"))

   ("To be, or not to be: that is the question:"
    "Whether 'tis nobler in the mind to suffer"
    "The slings and arrows of outrageous fortune,"
    "Or to take arms against a sea of troubles,"
    "And by opposing end them?"
    ("Hamlet" "III.1"))

   ("The first thing we do, let's kill all the lawyers."
    ("King Henry VI, Part 2" "IV.2"))

   ("You blocks, you stones, you worse than senseless things!"
    ("Julius Caesar" "I.1" "Marullus to the people of Rome"))

   ("The evil that men do lives after them"
    "The good is oft interred with their bones"
    ("Julius Caesar" "III.2"))

   ("He hath disgraced me, and hindered me half a million;"
    "laughed at my losses, mocked at my gains, scorned my nation,"
    "thwarted my bargains, cooled my friends, heated mine enemies;"
    "and what's his reason?  I am a Jew."
    ("Merchant of Venice" "III.1"))

   ("If you prick us, do we not bleed?"
    "if you tickle us, do we not laugh?"
    "if you poison us, do we not die?"
    "and if you wrong us, shall we not revenge?"
    ("Merchant of Venice" "III.1"))


   ("This royal throne of kings, this scepter'd isle,"
    "This earth of majesty, this seat of Mars,"
    "This other Eden, demi-paradise,"
    "This fortress built by Nature for herself"
    "Against infection and the hand of war,"
    ("King Richard II" "II.1"))

   
   ("There are more things in heaven and earth, Horatio,"
    "Than are dreamt of in your philosophy."
    ("Hamlet" "I.5"))


   ("My words fly up, my thoughts remain below:"
    "Words without thoughts never to heaven go."
    ("Hamlet" "III.4"))

   ("Love's not love"
    "When it is mingled with regards that stand"
    "Aloof from the entire point."
    ("King Lear" "I.1"))

   ("From women's eyes this doctrine I derive:"
    "They are the ground, the books, the academes,"
    "From whence doth spring the true Promethean fire."
    ("Love's Labour's Lost" "IV.3"))

   ("O brave new world that has such people in't!"
    ("The Tempest" "V.1"))

   ("But soft, what light through yonder window breaks?"
    "It is the east, and Juliet is the sun."
    ("Romeo and Juliet" "II.2"))

   ("If chance will have me king, why, chance may crown me"
    "without my stir."
    ("Macbeth" "I.3"))

   ("The course of true love never did run smooth"
    ("A Midsummer Night's Dream" "I.1"))

   ("As flies to wanton boys are we to the gods,"
    "They kill us for their sport."
    ("King Lear" "IV.1"))

   ("O, beware, my lord, of jealousy;"
    "It is the green-eyed monster, which doth mock"
    "the meat it feeds on."
    ("Othello" "III.3"))

   ("Yond Cassius has a lean and hungry look,"
    "He thinks too much; such men are dangerous."
    ("Julius Caesar" "I.2"))

   ("Shall I compare thee to a summer's day?"
    "Thou art more lovely and more temperate:"
    ("Sonnet 18"))

   ("Men were deceivers ever,"
    "One foot in sea, and one on shore,"
    "To one thing constant never."
    ("Much Ado About Nothing" "II.3"))

   ("Conscience does make cowards of us all,"
    "And thus the native hue of resolution"
    "Is sicklied o'er with the pale cast of thought."
    ("Hamlet" "III.1"))


   ]
  )


(provide 'shakespeare-quote)
;;;  shakespeare-quote.el ends here
