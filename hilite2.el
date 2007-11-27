From - Mon Feb 19 10:56:13 2001
Path: news.panix.com!panix!bloom-beacon.mit.edu!news-out.cwix.com!newsfeed.cwix.com!newsfeeds.belnet.be!news.belnet.be!skynet.be!newsfeed.esat.net!news.heanet.ie!not-for-mail
From: Brendan Halpin <brendan.halpin@ul.ie>
Newsgroups: gnu.emacs.help
Subject: Re: color all lines matching regexp?
Date: 15 Feb 2001 10:58:53 +0000
Organization: University of Limerick
Lines: 119
Message-ID: <m3ae7otdoy.fsf@wivenhoe.staff8.ul.ie>
References: <3A88701A.D3A5D5CD@deasil.com>
NNTP-Posting-Host: newscache.ul.ie
X-Trace: kenraki.heanet.ie 982238382 29734 136.201.1.36 (15 Feb 2001 11:59:42 GMT)
X-Complaints-To: news@kenraki.heanet.ie
NNTP-Posting-Date: 15 Feb 2001 11:59:42 GMT
X-Newsreader: Gnus v5.7/Emacs 20.5
Cache-Post-Path: newscache.ul.ie!unknown@wivenhoe.staff8.ul.ie
X-Cache: nntpcache 2.3.3 (see http://www.nntpcache.org/)
Xref: news.panix.com gnu.emacs.help:88288

Steve Wainstead <swain@deasil.com> writes:

> I have a lot of log output I need to comb over, and what would help is
> if I could have Emacs prompt me for a regexp, a color, and then apply
> that color to all matching lines in the region or the buffer. (Or all
> keywords).
> 
> Does such a feature exist? Haven't found it with finder-by-keyword or
> apropos. Does font-lock provide something like that?

I have a little bit of lisp that does something similar -- I wanted
to colour-code output from a stats program according to the
significance of each parameter in a model. It doesn't use
regexp-based search, but it iterates through the buffer/region
using plain Emacs Lisp.

To use, I highlight a region and issue the command "M-x sigem" (you
could equally do "M-: (sigem (point-min) (point-max))" to do the
whole buffer). This defun iterates through the region, calling
another defun (setcolsig) which tests each line and conditionally
does something to each line. In this case it looks at the numbers
in columns 46 to 51 (where the p-value happens to be) and colours
the line differently according to the value. 

This is customisable -- other tests, other treatments of the text. 

I suggest it for a number of reasons:
 - it's lightweight and adaptable
 - it's lisp, not regexp-based (of course, you could use regexps in
   the test)
 - it doesn't use font-lock stuff but sets colours "by hand"
 - maybe someone will suggest a better way to set colours than
   facemenu-set-{back|fore}ground! 

Brendan Halpin

PS p-whim-lock looks good too.


Here's the code; sample text to run it on is below:
;-------------------------------------------------------------------

(defun sigem (&optional b e)
"Iterate through region, do setcolsig to each line"
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (while (not (eobp))
        (setcolsig)
        (forward-line))))

(defun setcolsig ()
  (let ((p (string-to-number
            (buffer-substring (+ (point) 46)
                              (+ (point) 51))))
        (start (save-excursion (beginning-of-line) (+ 10 (point))))
;; Don't colour the variable name, only to right of column 10
        (end (save-excursion (end-of-line) (point))))
    (cond ((> p 0.10)              ; Not significant, colour grey
           (facemenu-set-background "Grey" 
                                    start
                                    end)
           (facemenu-set-foreground "Gray60" 
                                    start end))
          ((> p 0.05)              ; Weakly sig, colour black on grey
           (facemenu-set-foreground "Gray60" 
                                    start end))
          ((> p 0.01)              ; 0.05 >= sig > 0.01: blue on white
           (facemenu-set-foreground "blue" 
                                    start end))
          ((<= p 0.01)             ; p-val < 1%! Bold red on white
           (facemenu-set-foreground "red" 
                                    start end)
           (facemenu-set-face 'bold start end)))))
;-------------------------------------------------------------------

------------------------------------------------------------------------------
         |               Robust
   inocc |      Coef.   Std. Err.       z     P>|z|       [95% Conf. Interval]
---------+--------------------------------------------------------------------
  female |   1.302159   1.808299      0.720   0.471      -2.242042    4.846361
     age |  -.4038435   .5008005     -0.806   0.420      -1.385394    .5777075
IsXage_2 |  -.0496933    .074518     -0.667   0.505      -.1957459    .0963594
   agesq |   .0082933   .0101719      0.815   0.415      -.0116432    .0282298
Ipego_20 |  -.5467353   .4716367     -1.159   0.246      -1.471126    .3776557
Ipego_30 |   .1135606   .4418739      0.257   0.797      -.7524964    .9796175
Ipego_40 |  -.1972168   .7490166     -0.263   0.792      -1.665262    1.270829
Ipego_50 |   .0191474   .5253377      0.036   0.971      -1.010495     1.04879
Ipego_60 |  -.4079976   .5179901     -0.788   0.431       -1.42324    .6072444
Ipego_70 |   .1944959   .5813838      0.335   0.738      -.9449953    1.333987
Ipego_80 |  -.1091794   .6938814     -0.157   0.875      -1.469162    1.250803
Ipego_90 |   .2121485   .8337025      0.254   0.799      -1.421878    1.846175
Icldel_2 |   .0570587   .3089952      0.185   0.853      -.5485607    .6626782
Icldel_3 |  -.2928756   .4083364     -0.717   0.473        -1.0932    .5074491
Imarde_1 |  -1.326122   .5185735     -2.557   0.011      -2.342508    -.309737
Ipspo_20 |  -.2567633   .4344361     -0.591   0.555      -1.108242    .5947158
Ipspo_30 |  -.5683579   .4062628     -1.399   0.162      -1.364618    .2279026
Ipspo_40 |  -1.284516   .5785127     -2.220   0.026       -2.41838   -.1506519
Ipspo_50 |  -.7874204   .5007185     -1.573   0.116      -1.768811      .19397
Ipspo_60 |  -.7330481   .4603065     -1.593   0.111      -1.635232    .1691362
Ipspo_70 |  -1.189883   .6168969     -1.929   0.054      -2.398979    .0192128
Ipspo_80 |  -1.529067   .6393305     -2.392   0.017      -2.782132   -.2760024
Ipspo_90 |  -.2228803   .6867181     -0.325   0.746      -1.568823    1.123062
  ppaygu |   1.099129   .3656526      3.006   0.003       .3824635    1.815795
 psppayg |   .6792107   .2818292      2.410   0.016       .1268357    1.231586
   pkids |  -.5120946   .2747628     -1.864   0.062       -1.05062    .0264305
 getkids |  -.3709893   .4413703     -0.841   0.401      -1.236059    .4940805
Ipqfed_2 |   .3014652   .2793053      1.079   0.280      -.2459631    .8488936
Ipqfed_3 |  -.1227448   .2994032     -0.410   0.682      -.7095642    .4640747
  Iemp_2 |  -1.334798   .6818054     -1.958   0.050      -2.671112    .0015158
  Iemp_3 |  -.7770857   .4369752     -1.778   0.075      -1.633541    .0793699
   _cons |   3.389908   6.178505      0.549   0.583      -8.719739    15.49955
------------------------------------------------------------------------------

-- 
Brendan Halpin, Dept of Government and Society, Limerick University, Ireland
Tel: w +353-61-213147; f +353-61-202569; h +353-61-390476; Room S1-03 x 3147
<mailto:brendan.halpin@ul.ie>        <http://wivenhoe.staff8.ul.ie/~brendan>
