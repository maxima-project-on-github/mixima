;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mock MMA (A Lisp language mathematica-like system)
;;(c) copyright 1990, 1991 by Richard J. Fateman and Univ. of California
;; see enclosed notice (file copyright)

;; this file should be loaded in at COMPILE time for every file in
;; the mma package.  It should also be loaded in (once) when the
;; mma package is set up.


;; Mathematica, on which this is based,
;; is described in S. Wolfram: Mathematica, a
;; System for Doing Mathematics By Computer, (Addison-Wesley).

;; this line is not quite enough. Need to do, prior to compiling this

;; ok, I give up.  I will use CL standard losing case insensitivity.
;; Allegro had it right prior to the standard, but had to yield. The
;; result, that the case-sensitive stuff did not always work in later
;; releases, seems to  makes it advisable to just use case-insensitive-upper.
;; it also increases compatibility with other CLs  RJF. 10/30/97

;;;(eval-when (compile load eval)
;;;	   #+Allegro(cond((eq *current-case-mode* :case-sensitive-lower))
;;;		(t (excl::set-case-mode :case-sensitive-lower))))
  
  ;; obsolete (provide 'mma)

#-(or gcl clisp sbcl) (defpackage :mma (:nicknames "MockMMA") (:use :common-lisp :excl))
#+sbcl (defpackage :mma (:nicknames "MockMMA") (:use :common-lisp))
#+(or gcl clisp ) (defpackage :mma (:nicknames "MockMMA"))
  (in-package :mma)
  ;; this next line is not enough.. need to have these macros
  ;; available at compile time.
;;(declaim (ftype macro ulist uconsm))

(maxima::$load "uconsalt.lisp")
(defvar built-in-syms
  ;; these are the atoms used by the parser, evaluator, display,
  ;; etc.  They must be the same in each of the separate packages,
  ;; and so each package should be in this package ( :mma).
	  
    '(|AddTo| |Alias|
|Alternatives| ;; added 11/17/94
|And| |Apply| |Blank| |BlankNullSequence| |BlankSequence| 
|CompoundExpression| |Condition| |Delayed| |Derivative| |DivideBy| |Dot|
 |Equal| |Exit| 
|Factorial| |Factorial2| |False| |Function| |Greater| |GreaterEqual| |If| |In| |Increment|
|Inequality| |Integer| |Less| |LessEqual| |List| |Map| |MapAll| |MessageName|
      |NonCommutativeMultiply| |Not| |Null| |Optional| |Or| |Out| |Part| |Pattern|
|PatternTest| |Pi| |Plus| |Power| |PreDecrement| |PreIncrement| ;|PrimeQ| 
              |Put| |PutAppend|
      |Real| |Repeated| |RepeatedNull| |Replace| |ReplaceAll| |ReplaceRepeated|

      
|Rule| |RuleDelayed| |SameQ| |Sequence| |Set| |SetDelayed| |Slot| |SlotSequence|
      |SubtractFrom| |TagSet| |TagSetDelayed| |Times| |TimesBy| |True| |UnAlias| |Unequal|
     
|UnSameQ| |UnSet| |UpSet| |UpSetDelayed| |$Line| |Quote|)  ;; we added Quote.
  )

;; from eval
(eval-when (:compile-toplevel :load-toplevel :execute)(export '(mockmma mockmma2 mread1)))
