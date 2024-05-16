#|

  VERSION VERSIONNUMBER

  Copyright (C) 2008, John Lapeyre  All rights reserved.

  This program is free software; you can redistribute it
  and/or modify it only under the terms of the GNU General
  Public License version 2 as published by the Free Software
  Foundation.

  Mathematica is a registered trademark of Wolfram Research,
  Inc. (2008,2010)


 Code to translate Mma to Maxima. This code relies on
 Richard Fateman's parser (in a separate file) to produce pseudo-lisp
 code from Mma source code. The pseudo-lisp is then
 translated in two stages. The first stage is a translation
 to Maxima psuedo-lisp code. The second stage is to Maxima
 source code. In a diagram:

 Mma source -> Mma pseudo-lisp -> Maxima psuedo-lisp -> Maxima source

 IIRC, the pseudo-lisp Macsyma is very close to the lisp that Macsysma produces
 from Macsyma source.(check this.)
 Below, I often use 'intermediate' for 'psuedo-lisp'.

 The Macsyma source output is not formatted well.
 You can reformat function definitions like this for example 
 grind(fundef(macfunction));
   or actually just
 grind(macfunction);


|#

#|
   TODO
   * Allow choice of translation vs. compatibility functions eg ArcCos acos.

   * The dot operator does not behave the same in semi-complicated cases.
     Dot and . are fairly broken right now. Hmmm maybe best to use Dot afterall.
     Fix it anyway.  Note in rtest_mixima.mac notes on how Mma and macsyma differ here.
     Probably have to use Dot, because macsyma . does the wrong thing, except for
     simple vectors.

|#

; following 3 forms taken from mma.lisp
(if (find-package :maxima) t (defpackage :maxima))
(if (find-package :mma ) t (defpackage :mma (:nicknames "MockMMA") (:use :common-lisp
					      #+allegro     :excl
					      )))

;(defvar  maxima::|If|)

(in-package :mma)

; trying shadowing import breaks many things
#|
(shadowing-import
 '(
   maxima::MPLUS 
   maxima::MTIMES
   maxima::*MMINUS
   maxima::*UTIMES
   maxima::*MDIVIDE
   maxima::mexpt
   maxima::MSETQ
   maxima::MBLOB
   maxima::mequal
   maxima::MLESSP
   maxima::MGREATERP
   maxima::MGEQP
   maxima::MLEQP
   maxima::MAND
   maxima::MOR
   maxima::*MPART 
   maxima::MLIST 
   maxima::MPROGN 
   maxima::LAMBDA
   maxima::MDEFINE
   maxima::|Slot|
   maxima::|Pattern|
   maxima::|PreIncrement|
   maxima::|PreDecrement| 
   maxima::|Increment| 
   maxima::|Decrement| 
   maxima::|MessageName| 
   maxima::|Real|
   maxima::|AddTo| 
   maxima::|SubtractFrom|
   maxima::|MultiplyBy|
   maxima::|DivideBy|
   maxima::INPART 
   maxima::MARRAYREF
   maxima::OUT
   maxima::|For|
   maxima::|While| ))
|#
;   maxima::|If|  ; get an error if i try
   #|

|#


(setq blank-sequence-var-args 'blank-sequence-val-nil)
(setq lhs-intermediate-set 'lhs-intermediate-set-nil)
(setq lhs-source-set 'lhs-source-set-nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Hash Utility function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-hash-elements ( hash-table element-list) 
  "Set several hash elements at once from a list of pairs"
  (mapcar (lambda (pair) (setf (gethash (first pair) hash-table) (second pair))) element-list))

;; following two functions are for testing
(defun print-hash-entry (key value)
 (format t "key:~S, value:~S~%" key value))
;;  (format t "key:~a, value:~a~%" key value))

(defun dump-hash (hash-table)
  (maphash #'print-hash-entry hash-table))

; There must be a be builtin equivalent, but i cant find it.
; Returns a list of keys in the hash
(defun get-hash-keys (hash)
  (let ((klist ()))
    (maphash (lambda (x y) (declare (ignore y)) (setf klist (cons x klist))) hash)
    klist))

; add key-val pairs from hashto to pre-existing hashfrom
(defun merge-hash-tables (hashfrom hashto)
  (maphash (lambda (key val) (setf (gethash key hashto) val)) hashfrom))

; increment integer value of hash element. useful for making a list of unique elements
; as in perl
(defun increment-hash-value (key hash)
  (let ((valp (gethash key hash)))
    (cond ((not (eq nil valp)) (setf (gethash key hash) (+ valp 1)))
	  (t (setf (gethash key hash) 1)))))

;; "perl join". return a string given by elements of list lst separated by string sep
;; eg (pjoin ";" '(1,2,3)) --> "1;2;3"
(defun pjoin (sep lst) (format nil (concatenate 'string "~{~a~^" sep  "~}") lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  End of Hash Utility function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I don't understand if Mma has a clear lexical distinction between array and function.
;; These hashes will record appearances of array elements, so that later
;; they will (hopefully) not be confused with function.
;; This is known in theoretical computer science as a 'hack'.
(defvar *array-element-hash*  (make-hash-table :test #'equal  ))

(defvar *array-head-hash*  (make-hash-table :test #'equal  ))

;; code for storing and retrieving atom translation rule.
;; implementation as a hash table is hidden.
(defvar maxima::*translate-atom-table*
     (make-hash-table)  "Hash table to store translations of an atom to an atom")

; function names in apply need to be handled specially
; eg. MLIST can be a pair of [ ], but used with apply it is "["
(defvar *translate-apply-table*
     (make-hash-table)  "Hash table to store translations of argument to apply")

(defun set-apply-translation (vmma vmax)
  ( setf (gethash vmma *translate-apply-table*) vmax))

(defun get-apply-translation (vmma) 
  (gethash vmma *translate-apply-table*))

(defun set-apply-translation-list (trans-list) 
(mapcar (lambda (pair) (set-apply-translation (first pair) (second pair))) trans-list))

(defun set-intermediate-function-name-translation (vmma vmax)
  "Register the translaion of an mma atom to a maxima atom "
  ( setf (gethash vmma maxima::*translate-atom-table*) vmax))

(defun get-intermediate-function-name-translation (vmma) 
  "Retrieve the translation for an mma atom"
  (gethash vmma maxima::*translate-atom-table*))

(defun set-intermediate-function-name-translation-list (trans-list) 
 "Set several atom translations at once from a list of pairs"
(mapcar (lambda (pair) (set-intermediate-function-name-translation (first pair) (second pair))) trans-list))


;; Mma code is parsed into the symbols (elements?) in the left column below
;; The right column is an approximation of maxima equivalents.
;; --Pairs of elements giving translation of intermediate Mma operator to
;; intermediate macsyma operator. The intermediate Macsyma symbols that do not
;; have corresponding Mma FullForm symbols (or macsyma ?print form symbols)
;; are prefixed with * to show that they must be translated if one
;; wants full form. (Are these symbols, eg *MMINUS, symbols used in the internal Macsyma
;; lisp representation?)
;; Some of these identifiers are here to prevent clobbering built-in macsyma
;; identifiers, eg 'ident'
(set-intermediate-function-name-translation-list
 '(
;   (|Apply|         Apply) ; RF parser gave 'APPLY' for @@. GJL changed it to |Apply|
    ;  then let a comaptibility function handle it
;   (|Times|         MBLOB)
   (|Set|           MDUCK) ; not used evidently
   (|SetDelayed|    mdefine)
   (|Equal|         MEQUAL)
   (|Power|           MEXPT)
; Part is problematic. The mma parser returns symbol PART for [[]] notation and
; Part for Part. If both are translated to macsyma [] notation, then they can be used
; as lvalues. However, Part in Mma can have more sophisticated part specifications,
; and in this case, handling it as a compatibility function would be better.
; Making the translation depend on whether it is an lvalue would be good.

; The following two will be commented out and replaced by a handler that
; checks if we are in the LHS definition
;   (PART            *MPART) ; from [[ ]], not same as actual macsyma 'part'
;   (|Part|            *MPART) ; from Part[]. 
   (|Pattern|       |Pattern|)
   (Blank           Blank)
   (|*UTimes|         *UTIMES)
   (|*Divide|        *MDIVIDE)
   (|Plus|          mplus)
   (|Times|          mtimes) 
   (|*Minus|         *MMINUS)
   (FACTORIAL  |factorial|)
   (FACTORIAL2  |Factorial2|)
   (|Greater|       mgreaterp)
   (|Less|          MLESSP)
   (|GreaterEqual|  mgeqp)
   (|LessEqual|     mleqp)
   (|And|           MAND)
   (|Or|            MOR)
   (NOT           |not|) ; intermediate form is MNOT, but I don't have a generic
                       ; method written to translate that. So this is cheating
   (|List|          MLIST)  ; handle this in a function now
;;   (|List|          "[")  ; this is confusing. sometimes one, or the other
;   (|Dot|         MNCTIMES) ; prefer compatibility func over native Macsyma
   (|Dot|          |Dot|) ; use a compatibility function for this
;;   (|Module|        |block|)  ; move module and block to lisp functions
;;   (|Block|         |block|)  ; 
   (RULE  |Rule|)
   (MAP  |Map|)
   (REPLACEALL  |ReplaceAll|)
   (|CompoundExpression|   MPROGN)
   ))

; qualifying 'MMA::BLOB like this does not work. It still appears
; in the hash table as MBLOB. But, source-infix-ops-table puts
; the symbol MBLOB in :mma unless it is specifically prefixed otherwise
; I dont know if this is a bug or something I dont understand, or ...
; There is no problem (with gcl in maxima) if I call functions while
; (in-package :mma), but not if I am in :maxima

; disable this I guess. Now handling Times as alias....
;(set-intermediate-function-name-translation '|Times| 'MMA::MBLOB)

;; User defined functions in Mma user code that would clobber
;; built-in macsyma functions (eg ident in some example user code)
;; Eg: A user writes pure Mma code, and uses legal identifiers that happen
;; to be the same as Macsyma builtin identifiers. If translated directly to
;; Macsyma, this will cause a collision.
;; Probably all macsyma functions should be included here. Currently, some functions
;; that occured in actual examples of user code are included here.
(defvar *no-clobber-table* (make-hash-table))
(set-hash-elements *no-clobber-table*
      '(
	(|ident| |mma_user_ident|)
	(|transpose|  |mma_user_transpose|)
	(|adjoint| |mma_user_adjoint|)
;        ( MPLUS  "+") ; following four are not picked upt
;        ( MTIMES  "*") ; not useful here. remove soone
;        ( mplus "+")
;        ( mtimes  "*")
	))

; for now handle these two tables with the same routines
(merge-hash-tables *no-clobber-table* maxima::*translate-atom-table*)

; does not seem to make a difference
;(defvar source-infix-ops-table  (make-hash-table))
(setq maxima::source-infix-ops-table  (make-hash-table))

;; Table used to translate intermediate-macsysma to Macsyma symbols
;; These are searched before source function table below.
;; If handling an infix is more complicated than inserting the symbol,
;; then write a macsyma-source-foo handler below.
(set-hash-elements maxima::source-infix-ops-table
	'( 
	  ( MPLUS "+" )
          ( MTIMES "*" )
	  ( *MMINUS "-" )
	  ( *UTIMES "*" ) ; this should be converted to just "-", but it is not wrong
	  ( *MDIVIDE "/" )
	  ( mexpt "^" )
;	  ( msetq ":" )
;	  ( mdefine ":=" ) ; must switch this back from func
	  ( MSETQ "=>" )
	  ( MBLOB "*" )
;	  ( qmtimes "*" )
;	  ( MBLOB "*" )
	  ( mequal "=" )
	  ( MLESSP "<" )
;	  ( |List| "[" )
	  ( MGREATERP ">" )
	  ( MGEQP ">=" )
	  ( MLEQP "<=" )
	  ( MAND   " and " )
	  ( MOR   " or " )
	 ;( MNCTIMES  " . ") ; Use a compatibility function for this instead
))

;  test removing this in favor of the import list above
; working around problem with calling from maxima
(set-hash-elements maxima::source-infix-ops-table
	'( 
	  ( maxima::MPLUS "+" )
          ( maxima::MTIMES "*" )
	  ( maxima::*MMINUS "-" )
	  ( maxima::*UTIMES "*" ) ; this should be converted to just "-", but it is not wrong
	  ( maxima::*MDIVIDE "/" )
	  ( maxima::mexpt "^" )
;	  ( mdefine ":=" ) ; must switch this back from func
	  ( maxima::MSETQ "=>" )
	  ( maxima::MBLOB "*" )
	  ( maxima::mequal "=" )
	  ( maxima::MLESSP "<" )
;	  ( |List| "[" )
	  ( maxima::MGREATERP ">" )
	  ( maxima::MGEQP ">=" )
	  ( maxima::MLEQP "<=" )
	  ( maxima::MAND   " and " )
	  ( maxima::MOR   " or " )
))
;

;; These are lisp functions that write Macsyma source code
;; to implement the corresponding intermediate Macsyma function each time it is needed. 
;; For instance
;; the entry for 'For' will write a Macsyma 'for' statement.
;; !!! should be a defvar or something
(setq *source-function-table*  (make-hash-table))
(set-hash-elements *source-function-table*
	'( 
	  (*MPART macsyma-source-mpart) ; in current version should never be used .
	  (MLIST  macsyma-source-mlist)
	  (MPROGN macsyma-source-mprogn)
;	  (LAMBDA macsyma-source-lambda) ; ultimately, we need a mix of methods  1 and 2
	  (LAMBDA macsyma-source-lambda2)
;	  (|Slot| macsyma-source-slot)
;;	  (|Slot| macsyma-source-slot2)
          (SLOTSEQUENCE macsyma-source-slot-sequence)
          (MDEFINE macsyma-source-mdefine)
	  (|Pattern| macsyma-source-pattern)
;;	  (|PreIncrement| macsyma-source-preincrement) ; have functions, but they don't work with some code.
;;	  (|PreDecrement| macsyma-source-predecrement)
;;	  (|Increment| macsyma-source-increment)
;;	  (|Decrement| macsyma-source-decrement)
;;	  (|AddTo| macsyma-source-add-to)
;;	  (|SubtractFrom| macsyma-source-add-to)
;;	  (|MultiplyBy| macsyma-source-add-to)
;;	  (|DivideBy| macsyma-source-add-to)
	  (|MessageName|  macsyma-source-messagename)
	  (|Real| macsyma-source-Real)
	  (OUT macsyma-source-out) ; %, %%, etc.
;;	  (|For| macsyma-source-for)  ;; try out the For and While functions
;;	  (|While| macsyma-source-while)
;;	  (|If| macsyma-source-if)
	  (INPART macsyma-source-inpart) ;; this now calls Part below
	  (MARRAYREF macsyma-source-marrayref)
))

; repeated to fix when calling from maxima
(set-hash-elements *source-function-table*
	'( 
	  (maxima::*MPART macsyma-source-mpart) ; in current version should never be used .
	  (maxima::MLIST  macsyma-source-mlist)
	  (maxima::MPROGN macsyma-source-mprogn)
;	  (LAMBDA macsyma-source-lambda) ; ultimately, we need a mix of methods  1 and 2
	  (maxima::LAMBDA macsyma-source-lambda2)
;	  (|Slot| macsyma-source-slot)
;;	  (maxima::|Slot| macsyma-source-slot2)
          (maxima::SLOTSEQUENCE macsyma-source-slot-sequence)
          (maxima::MDEFINE macsyma-source-mdefine)
	  (maxima::|Pattern| macsyma-source-pattern)
;;	  (maxima::|PreIncrement| macsyma-source-preincrement)  ; do with functions now, except see For loop below
;;	  (maxima::|PreDecrement| macsyma-source-predecrement)
;;	  (maxima::|Increment| macsyma-source-increment)
;;	  (maxima::|Decrement| macsyma-source-decrement)
;;	  (maxima::|AddTo| macsyma-source-add-to)
;;	  (maxima::|SubtractFrom| macsyma-source-add-to)
;;	  (maxima::|MultiplyBy| macsyma-source-add-to)
;;	  (maxima::|DivideBy| macsyma-source-add-to)
	  (maxima::|MessageName|  macsyma-source-messagename)
	  (maxima::|Real| macsyma-source-Real)
	  (maxima::OUT macsyma-source-out) ; %, %%, etc.
;;	  (maxima::|For| macsyma-source-for)
;;	  (maxima::|While| macsyma-source-while)
;;	  (maxima::|If| macsyma-source-if)
	  (maxima::INPART macsyma-source-inpart)
	  (maxima::MARRAYREF macsyma-source-marrayref)
))


; using functions for this
; Handle operators like +=, -=, etc.
(defvar *add-to-table*  (make-hash-table))
(set-hash-elements *add-to-table*
      '(
	(|AddTo|        "+")
	(|SubtractFrom| "-")
	(|MultiplyBy|   "*")
	(|DivideBy|     "/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN macsyma-intermediate-... translation and hander functions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; table of handlers for translations of Mma pseudo-lisp to Macsyma pseudo-lisp.
(defvar *intermediate-function-table*  (make-hash-table))
(set-hash-elements *intermediate-function-table*
	'( 
;;	  (|Function|  macsyma-intermediate-Function)  try using builtin
	  (|Set|  macsyma-intermediate-Set)
	  (|Pattern|  macsyma-intermediate-Pattern)
	  (|Part|  macsyma-intermediate-Part)
	  (PART  macsyma-intermediate-Part)
	  (|SetDelayed|  macsyma-intermediate-SetDelayed)))

;; Top level function for Mma intermediate to Macsyma intermediate.
;; Also called recursively for subexpressions.
;; Convert an intermediate (pseduo-lisp) Mma expression into intermediate
;; Macsyma expression.
(defun to-macsyma-expr (mma-expr)
 "Convert the s-expression 'mma-expr' to a macsyma expression"
; (cond ((listp mma-expr) (check-for-split-set mma-expr)))
 (cond 
   ((symbolp mma-expr)
    (let ((trans (get-intermediate-function-name-translation mma-expr))) ; look for translation for this expression
      (cond (trans (intern (format nil"~a" trans))) ;found a translation
	    (t (intern (format nil"~a" mma-expr)))))) ;; don't add dollar sign
	;(t (intern (format nil"$~a" mma-expr)))))) ;; a symbol foo -> $foo or |$foo| perhaps
   ((atom mma-expr) mma-expr) ; return unchanged atom, if we don't know what else to do.
   ((intermediate-handle-functions mma-expr) )
   (t (macsyma-intermediate-default mma-expr))))
;   (t (mapcar #'to-macsyma-expr mma-expr)))) ; map over list if we don't know what else to do.

  
;; First look up function to translate pseudo-lisp output of Mma source parser 
;; into intermediate Macsyma form.
;; Then call the function on args to write the code and return the result, a lisp list.
;; Since, both forms are lisp-like, usually no work needs to be done. This function is
;; called for the few more complicated cases.
(defun intermediate-handle-functions (expr)
;  (format t "Handling intermediate ~a ~%" expr)
  (let ((hfunc (gethash (first expr) *intermediate-function-table*)))
;    (format t "got func intermediate ~a ~%" hfunc)
    (cond ( hfunc (funcall hfunc expr))
	  (t nil))))


; collect unique slot numbers as keys in a hash
; the hash values are irrelevant
(defun tally-slots (expr hash)
  (cond
    ((atom expr) nil)
    ((eq '|Slot| (first expr))
     (setf (gethash (second expr) hash) t))
; this doesn't really work, so we will fix it when translating to source
;     (setf expr (intern (format nil "arg~d" (second expr))))
;     (format t "HI~a~%" expr))
    (t  ;(format t "got list ~a~%" (first expr))
     (mapcar (lambda (x) (tally-slots x hash)) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macsyma-intermediate-... functions. These handlers translate
;; particular parts of the Mma pseudo-lisp to Macsyma psuedo-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun macsyma-intermediate-Pattern (expr)
  (cond ( (and (listp (third expr )) (eq  (first (third expr)) 'BLANKSEQUENCE))
	  (setq blank-sequence-var-args (second expr)))) ; name for variable number of args
  expr)


;; commented out code here, and the following function is start of
;; attempt to implement pattern matching in function definition.
;; That is  mmafunc[x,{a,b}] :=, becomes
;; block([blah]  tellsimp(mmafunc(x,[a,b]),internal_mmafunc([args])), etc.
(defun macsyma-intermediate-SetDelayed (expr &aux res)
  (setq blank-sequence-var-args 'blank-sequence-val-nil)
;  (cond ((every #'atom (rest (second expr)))
;	 (setq res (macsyma-intermediate-pattern-args expr)))
;	(t
;	 (setq res (mapcar #'to-macsyma-expr expr))))
  (setq res (mapcar #'to-macsyma-expr expr))
  (setq blank-sequence-var-args 'blank-sequence-val-nil)
  res)
	 
(defun macsyma-intermediate-pattern-args (expr &aux res)
;  (format t "in pattern args~%")
  (setq res  (mapcar #'to-macsyma-expr expr))
  (setq res (cons  'mprogn (list '(|block|
		     (mlist (msetq |simp| |false|))) res)))
  res)

#|
  msetq arrayref part inpart =>, etc. . this is the old way. Now we handle it
 at runtime with a function mixSetQ
(defun macsyma-intermediate-Part (expr)
  (cond ((eq lhs-intermediate-set 'lhs-intermediate-set-nil)
	 (cons 'INPART (mapcar #'to-macsyma-expr (rest expr))))
	(t (cons 'MARRAYREF (mapcar #'to-macsyma-expr (rest expr))))))
|#

;; new way that relies on =>
(defun macsyma-intermediate-Part (expr)
  (cons 'INPART (mapcar #'to-macsyma-expr (rest expr))))


;; Anonymous (ie lambda) functions:
;; Replace Mma (Function.. by Macsyma (LAMBDA (arg1 arg2..) ...
;; the Slot(x) remain the same and must be handled on printing.
;; This should be cleaned up at some point
;; Also, arg1, etc. should be replaced with something like
;; gensym, although this does not work at the maxima source level.
(defun macsyma-intermediate-Function (expr)
  (let ((slots (make-hash-table)) (args))
    (tally-slots expr slots)
    (setf args (mapcar (lambda (x) (intern (format nil "arg~d" x)))
		       (sort (get-hash-keys slots) #'<)))
    (cons 'LAMBDA (cons args (mapcar #'to-macsyma-expr (rest expr))))))

; NOT USED
(defun macsyma-intermediate-Function2 (expr)
  (let ((slots (make-hash-table)) (args))
    (tally-slots expr slots)
    (setf args (mapcar (lambda (x) (intern (format nil "arg~d" x)))
		       (sort (get-hash-keys slots) #'<)))
    (cons 'LAMBDA (cons args (mapcar #'to-macsyma-expr (rest expr))))))

;; Write intermediate Macsyma Set code from intermediate Mma.
;; The lhs of Set should usually be an array element (or part of list),
;; if it looks like an Mma function call. But there are some exceptions.
;; This works for cases encountered thus far.
;; If the lhs is not an atom, assume it is an array element. Instead of
;; using macsyma's internal notation for an array, we use *MPART, which
;; is invented just for this translator.
;; Becuase it is based on patterns, Mma allows function definition, eg bell[x_] := , to coexist with
;; definitions for specific values, eg bell[0] = ...
(defun macsyma-intermediate-Set (expr)
  (let ( (lhs (second expr)) (rhs (third expr))
	 (newlhs ()))
    (setq lhs-intermediate-set 'lhs-intermediate-set-true)
    (setq newlhs (to-macsyma-expr (second expr)))
    (setq lhs-intermediate-set 'lhs-intermediate-set-nil)
    (cond
      ( (or (atom lhs) (eq (first lhs) '|MessageName|)
	    (eq (first newlhs) '*MPART))
;       (format t "~a~%" lhs)
       (cons 'MSETQ (mapcar #'to-macsyma-expr (rest expr))))
      (t (increment-hash-value lhs *array-element-hash*)
 (increment-hash-value (first lhs) *array-head-hash*)
	 ;(dump-hash *array-element-hash*)
	 (cons 'MSETQ (cons newlhs
			    (list (to-macsyma-expr rhs))))))))
;	 (cons 'MSETQ (cons (cons '*MPART newlhs) 
;			    (list (to-macsyma-expr rhs))))))))

(defun disable-good-macsyma-intermediate-Set (expr)
  (let ( (lhs (second expr)) (rhs (third expr))
	 (newlhs (to-macsyma-expr (second expr))))
    (cond
      ( (or (atom lhs) (eq (first lhs) '|MessageName|)
	    (eq (first newlhs) '*MPART))
;       (format t "~a~%" lhs)
       (cons 'MSETQ (mapcar #'to-macsyma-expr (rest expr))))
      (t (increment-hash-value lhs *array-element-hash*)
	 (increment-hash-value (first lhs) *array-head-hash*)
	 ;(dump-hash *array-element-hash*)
	 (cons 'MSETQ (cons (cons '*MPART newlhs) 
			    (list (to-macsyma-expr rhs))))))))

; For translation of arguments to apply
(set-apply-translation-list
 '(
   (|List| "[")
   (|Times|   MTIMES)))

; If there is a blank-sequence in the argument list, then we need to
; simulate pattern matching using 'apply rather than a function call
; wherever the pattern occurs in the function definition
;  only called as default ?
(defun macsyma-intermediate-function-call (expr &aux apfun trans)
;;  (format t "  macsyma-intermediate-function-call  ~a~%" expr)
;;  (format t "   var-args ~a~%" blank-sequence-var-args)
  (cond ( (and (> (length expr) 1 ) (some #'(lambda (x) (eq blank-sequence-var-args x)) expr))
	  (setq trans (get-apply-translation (first expr))); fix this. necessary ? 
	  (cond (trans (setq apfun trans)) ;found a translation
		      (t (setq apfun (first expr))))
	  (cons '|apply|
		(list apfun
		      (cons '|append|
		      (mapcar (lambda (x) (cond ( (eq x blank-sequence-var-args) x) (t (list 'mlist x)))) 
			      (mapcar #'to-macsyma-expr (rest expr)))))))
	(t  (mapcar #'to-macsyma-expr expr) ))) ; map over list if we don't know what else to do.

(defun macsyma-intermediate-default (expr)
  (cond ((symbolp (first expr))
	 (macsyma-intermediate-function-call expr))
	(t  (mapcar #'to-macsyma-expr expr) ))) ; map over list if we don't know what else to do.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End macsyma-intermediate-... functions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Macsyma intermediate to Macsyma source translation and handler functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First look up function to translate intermediate Macsyma to macsyma code.
;; Then call the function on args to write the code and return the result, a string
;; of Macsyma source. Eg, this will write a Macsyma 'for' or 'if'  statment.
(defun source-handle-functions (expr)
;  (format t "Handling '~a'~%" expr)
  (let ((hfunc (gethash (first expr) *source-function-table*)))
;    (format t "got func ~a ~% " hfunc)
    (cond ( hfunc (funcall hfunc expr))
	  (t nil))))

; Treat the top level of the source slightly differently than expressions
; at a lower level.
; MPROGN is treated differently if we are at the file level and not
; in a function definition (or otherwise desired block. This was
; really a series of statements at the file level. Hmm i think this
; is wrong. The problem is, often a single Mma statement ending with ;
; is called compoundstatement. it should be translated as a single
; macsyma statement, not a (..,..) with only one element.
; This code seems to be working for now, but check that it does not
; cause problems...
(defun macsyma-source-start (expr)
;  (format t "Starting converseion ~%")
  (cond
    ((eq 'MPROGN (first expr))
     (pjoin ";" (mapcar #'macsyma-source (rest expr))))
    (t (concatenate 'string  (macsyma-source expr) ";"))))


;; Translate an expression, with subexpressions of
;; intermediate Macsyma into Macsyma source. This calls
;; various routines to translate different kinds of elements
;; differently.
(defun macsyma-source ( expr ) 
;  (cond ((listp expr) (setf expr (remove '$NULL expr :test #'equal ))))
;  (format t "converting ~a ~%" expr)
  ( cond 
 ;   ((equal '$NULL expr) "rnul")
    ((symbolp expr)  (return-from macsyma-source  (macsyma-source-symbol expr)))
    ((stringp expr)  (return-from macsyma-source  (format nil "~s" expr)))
    ((atom expr)  (return-from macsyma-source  (format nil "~a" expr)))
    ((listp expr)
     (cond ((listp (first expr)) ; top-level list
	    ( macsyma-source-top-level-list expr))
	   (t 
	    (let ((op (gethash (first expr) maxima::source-infix-ops-table)))
					;(print (format nil ">>~S<< >>~S<<~%" (first expr) op))
;	      (format t "got infix op  >>~a<< for >>~a<< ~%" op (first expr))
;	      (format t "got infix op  >>~S<< for >>~S<< ~%" op (first expr))
	      ( cond
		(op (macsyma-source-nary-infix op expr))
;		( (eq 'MLIST (first expr)) (macsyma-source-mlist expr))
;		( (eq '*MPART (first expr)) (macsyma-source-mpart expr))
		((source-handle-functions expr) )
		( t (macsyma-source-default expr)))))))))


;; Following several functions write Macsyma source from Macsyma intermediate s-expressions.
;; I think the increment-decrement hack is here. IIRC R. Fateman once suggested a way around this
;; The assignment part is written explicitly here rather than using the generic infix translation
;; because Maxima gives an error with things like "for (i:1) ... "
;; We could also use a compatiblity function for For, but issues with if, when, and how things
;; are evaluated
(defun macsyma-source-for (expr)
  (let( (initc 
	 (format nil "(~a):(~a)" (macsyma-source (second (second expr)))
		 (macsyma-source (third (second expr)))))
	(testc (macsyma-source (third expr)))
	(oincrc (fourth expr))
	(bodyc (macsyma-source(fifth expr)))
	(incrh) (step) )
    (setf incrh (first oincrc))
    ( cond ( (or (eq incrh '|PreIncrement|) (eq incrh '|Increment|)  (eq incrh 'maxima::|PreIncrement|)  (eq incrh 'maxima::|Increment|))
	    (setf step 1))
	   ( (or (eq incrh '|PreDecrement|)  (eq incrh '|Decrement|) (eq incrh 'maxima::|PreDecrement|)  (eq incrh 'maxima::|Decrement|))
	    (setf step -1)))
    (format nil "for ~a while ~a step ~a do (~a)~%"
	    initc testc step bodyc)))


(defun macsyma-source-while (expr)
  (let( (testc (macsyma-source (second expr)))
	(bodyc  (macsyma-source (third expr))))
	(format nil "while ~a do (~a)~%" testc bodyc)))
    
;; This needs to be improved to handle the case in which the
;; predicate is neither true nor false. In Mma, this is a fourth
;; clause.
(defun macsyma-source-if (expr)
  (let (
	(elen (length expr))
	(testc (macsyma-source (second expr)))
	(truec (macsyma-source (third  expr)))
	(falsec)
	(maybec))
    (cond 
      ( (> elen 3) (setf falsec (macsyma-source (fourth expr)))))
    (cond 
      ( (> elen 4) (setf maybec (macsyma-source (fifth  expr)))))
    (cond
      ((eq elen 3)
       (format nil " if ~a then (~a)" testc truec))
      ((eq elen 4)
       (format nil " if ~a then (~a) else (~a)" testc truec falsec))
      ((eq elen 5)
       (format nil " if (maybe(~a) = true) then (~a) elseif (maybe(~a)= false) then (~a) else (~a)"
	       testc truec testc falsec maybec)))))

; Symbol '%' for previous ouput
;  % -> %
;  %n -> %on  for n>0
;  %% -> %th(2)
;  (OUT n) -> %th(abs(n)) for n<0
; ehh. this is handled with a function. so maybe disable this ...
(defun macsyma-source-out (expr)
  (cond ( (eq (length expr) 1)
	  (format nil "%" )) ;  (OUT)
	( (> (second expr) 0)
	 (format nil "%o~a" (macsyma-source (second expr))))
	(t
	 (format nil "%th(~a)" (macsyma-source (abs (second expr)))))))

(defun macsyma-source-array-element (expr)
  (format nil "~a[~a]" (macsyma-source (first expr))
	  (pjoin "," (mapcar #'macsyma-source (rest expr)))))

; dont use this
#|
(defun disable-macsyma-source-function-call (expr)
  (cond ( (eq (second expr) blank-sequence-var-args)
	  (format nil "apply('~a,~a)" (macsyma-source (first expr))
		(pjoin "," (mapcar #'macsyma-source (rest expr)))))
	( t
	  (format nil "~a(~a)" (macsyma-source (first expr))
		(pjoin "," (mapcar #'macsyma-source (rest expr)))))))
|#

;; Default way to call a function
(defun macsyma-source-function-call (expr)
  (format nil "~a(~a)" (macsyma-source (first expr))
	  (pjoin "," (mapcar #'macsyma-source (rest expr)))))

; Handle patterns.
; For single underscores, simply omit them
; For double underscores, ...
(defun macsyma-source-pattern(expr &aux (arg (third expr)) )
  (cond
    ( (or (not (listp arg)) (eq  (first arg) 'BLANK))
     (format nil "~a"  (macsyma-source-symbol (second expr))))
    ( (eq  (first arg) 'BLANKSEQUENCE)
     (setq blank-sequence-var-args (second expr)) ; name for variable number of args
     (format nil "[~a]"  (macsyma-source-symbol (second expr))))))


; The following two will fail in some cases; When mixing # with ##, I think.
; Mma allows passing extraneous arguments to a lambda function. Macsyma does not.
; We handle it by making every Macsyma lambda function take variable-number of args
; (ie they all go into a list lambda_args.
; Then, when the args are references, ie Slot 1, Slot 2, etc, take the appropriate element
; from list of arguments.
;; try evaluating the args. This did not cause an error yet.
(defun macsyma-source-lambda2(expr)
  (format nil "lambda( [[lambda_args]],  ~a )"
;;  (format nil "lambda( [[lambda_args]], lambda_args : map( 'ev, lambda_args ),  ~a )" 
	  (pjoin "," (mapcar #'macsyma-source (rest(rest expr))))))

(defun macsyma-source-slot2(expr)
  (format nil "lambda_args[~a]" (second expr)))

;; This is not correct. We need to splice lambda_args in,
;; or apply the surrounding code somehow. This was working in
;; a previous version, I thought...
;; NOTE: I've attempted to do something vaguely sensible. RHD 2024-05-16
(defun macsyma-source-slot-sequence(expr)
  (format nil "lambda_args[~{~a~^,~}]" (second expr)))

; not used now
(defun macsyma-source-lambda(expr)
  (format nil "lambda( [~{~a~^,~}], ~a )" (second expr)
	  (pjoin "," (mapcar #'macsyma-source (rest(rest expr))))))

; not used now
(defun macsyma-source-slot(expr)
  (format nil "arg~a" (second expr)))

(defun macsyma-source-mpart(expr) ; should never be called now
  (format nil "~a~{[~a]~}"  (macsyma-source (second expr))
	   (mapcar #'macsyma-source (rest (rest expr)))))

(defun macsyma-source-marrayref(expr)
  (format nil "~a~{[~a]~}"  (macsyma-source (second expr))
	   (mapcar #'macsyma-source (rest (rest expr)))))

(defun macsyma-source-mlist (expr)
  (format nil "[~a]" 
	  (pjoin "," (mapcar #'macsyma-source (rest expr)))))

(defun macsyma-source-lhs (expr &aux res)
  (setq lhs-source-set 'lhs-source-set-t)
  (setq res (macsyma-source expr))
  (setq lhs-source-set 'lhs-source-set-nil)
  res)

; the lh thing in this func needs to be applied to the appropriate funcs below.
; actually, we need to go back the intermediate form and recode increments there,
; because they dont exist in macsyma and it is cleaner and more consistent than
; doing it here.
(defun macsyma-source-preincrement (expr)
  (let ((var-lh (macsyma-source-lhs(second expr)))
	(var-rh (macsyma-source(second expr))))
    (format nil "((~a):(~a)+1)" var-lh var-rh))) 

(defun macsyma-source-predecrement (expr)
  (let ((var-lh (macsyma-source-lhs(second expr)))
	(var-rh (macsyma-source(second expr))))
    (format nil "((~a):(~a)-1)" var-lh var-rh))) 

(defun macsyma-source-increment (expr)
  (let ((var-lh (macsyma-source-lhs(second expr)))
	(var-rh (macsyma-source(second expr))))
    (format nil "((~a):(~a)+1,(~a)-1)" var-lh var-rh var-rh))) 

(defun macsyma-source-decrement (expr)
  (let ((var-lh (macsyma-source-lhs(second expr)))
	(var-rh (macsyma-source(second expr))))
    (format nil "((~a):(~a)-1,(~a)+1)" var-lh var-rh var-rh))) 

; not called. using functions for this
(defun macsyma-source-add-to (expr)
  (format nil "~a:~a ~a ~a" (macsyma-source-lhs (second expr)) (macsyma-source (second expr))
	  (gethash (first expr) *add-to-table*)
	  (macsyma-source (third expr))))

(defun macsyma-source-Real (expr)
  (format nil "~d" (+ (second expr) (float (third expr)))))

; put parens around comma separated list of statements
; remove NULL statements occur frequently in compound
; statement from Mma parser. I don't know if will ever break something.
(defun macsyma-source-mprogn (expr)
;  (setf expr (remove 'NULL expr))
  (setq expr (remove 'NULL expr))
  (format nil "(~a)" 
	  (pjoin "," (mapcar #'macsyma-source (rest expr)))))

(defun macsyma-source-messagename (expr)
  (format nil "~a_~a" (macsyma-source-symbol (second expr)) (macsyma-source-symbol (third expr))))
;	  (pjoin "," (mapcar #'macsyma-source (rest expr)))))

; Wite an expression with nary infix (including binary)
; operator eg: ( a + b + 12 ) the mma parser does not note
; where parens were included in Mma source to override
; precedence. But it does build s-expressions with proper
; precedence.  So we put parens around every expression,
; rather than try to implment macsyma precedence rules The
; maxima reader (or some part of the Macsyms program) will
; remove superfluous parens when echoing the source.
(defun macsyma-source-nary-infix (op expr)
  "print a nary infix operator expression (can also be binary.)"
  (format nil "(~a)"   (pjoin op (mapcar #'macsyma-source (rest expr)))))
;  (pjoin op (mapcar #'macsyma-source (rest expr))))

; what is this? I think something to treat file differently than compound expression.
(defun macsyma-source-top-level-list (expr)  ; this is not being used ? Yes it is
;  (mapcar #'macsyma-source expr))
; (pjoin "<;>" (mapcar #'macsyma-source expr)))
;  (concatenate 'string "Top Level --> " (pjoin " " (mapcar #'macsyma-source expr))))
  (concatenate 'string "" (pjoin " " (mapcar #'macsyma-source expr))))
; (pjoin "<;>" (mapcar #'macsyma-source (remove "NULL" expr :test #'equal ))))

;; Symbols here are translated if they were not already translated differently
;; as the head of a list.
;; MTIMES should only be picked up here when it occurs as an
;; argument and not as the head of a list (ie not when it appeared in Mma
;; as an infix operator.
;;
;; This package qualifier thing is a pain. Maybe make a list somewhere
;; of symbols and import them
(defvar *special-symbol-table*  (make-hash-table :test #'equal ))
(set-hash-elements *special-symbol-table*
	'( 
          (MTIMES |"*"|) ; this actually works
          (MPLUS |"+"|)
          (MLIST |List|)
	  (|Pi| |%pi|)
	  (|Infinity| |inf|)
	  (|ComplexInfinity| |infinity|)
	  (E |%e|)
	  (I |%i|)
          (maxima::MLIST |List|)
          (maxima::MTIMES |"*"|) ; this actually works
          (maxima::MPLUS |"+"|)
	  (maxima::|Pi| |%pi|)
	  (maxima::|Infinity| |inf|)
	  (maxima::|ComplexInfinity| |infinity|)
	  (maxima::E |%e|)
	  (maxima::I |%i|)
          ))

(defun macsyma-source-special-char (sym &aux newsym)
  (setf newsym (gethash sym *special-symbol-table* ))
;  (format t "Checking ~s and got ~s~%" sym newsym)
  (cond
   ( newsym newsym )
   (t  sym )))

;; input is lisp symbol, output is string representing macsyma symbol
(defun macsyma-source-symbol (sym &aux nsym)
 "translate lisp or lisp-like symbol into maxima symbol (as a string)"
 (cond ((equal 'NIL sym)  (return-from macsyma-source-symbol (format nil "~%"))  ))
 (cond ((equal 'NULL sym)  (return-from macsyma-source-symbol (format nil "~%")  )))
 (setf nsym (macsyma-source-special-char sym))
; (format t "trying to find source for symbol ~a, got ~a~%" sym nsym)
 (let ((ssym (string nsym)))
   ssym))


(defun macsyma-source-mdefine (expr &aux res)
  ; start  data structure here saying we are starting a func definintion
  ; it will have info about patterns
;  (setq function-definition t)
;  (setq blank-sequence-var-args nil)
;;  (format t " entering macsyma-source-mdefine ~a, bsa:'~a' ~%" expr blank-sequence-var-args )
  (setq res (macsyma-source-nary-infix ":=" expr))
;;  (format t " leaving macsyma-source-mdefine ~a, bsa:'~a' ~%" expr blank-sequence-var-args )
;  (setq function-definition nil)
  (setq blank-sequence-var-args 'blank-sequence-val-nil ) ; for some reason this line was commented out. caused a bug
  res
   ; clear the data structure because the definition is now written
   ; and we need to forget about 'local variables' (ie patterns)
     )

(defun macsyma-source-inpart (expr)
  (if (eq lhs-source-set 'lhs-source-set-nil)
      (macsyma-source-default (cons '|Part| (rest expr)))
    (macsyma-source-marrayref expr)))
    

;; This is the default
;; that happens when there is no rule for translating a
;; function.  If the head is not a symbol, then call top
;; level of translation on each element.  If the head is a
;; symbol, then write a function call. But first, look to
;; see if we think the head is for an array in which case we
;; use [] instead of ()
(defun macsyma-source-default (expr)
  (cond ((symbolp (first expr))
;;         (format t "expr ~s, array-head-h ~s~%" (first expr) (gethash (first expr) *array-head-hash*))
         (cond
          ((gethash (first expr) *array-head-hash*) (macsyma-source-array-element expr))
          (t  (macsyma-source-function-call expr))))
	(t (pjoin "~%" (mapcar #'macsyma-source expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END  Macsyma intermediate to Macsyma source translation and handler functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Steering functions. Called by scripts to do translation and I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
; input -- filename of file of Mma source
; output -- lisp list of intermediate Mma s-expression
; pslst is defined in mixima-mockmma-parser ie, it calls the (modified) Fateman parser
; it reads and parses maxima code from a stream
; and returns pseudo-lisp
(defun psl (fname)
  (pslst (open fname)))

; input -- filename of file of Mma source
; output -- lisp list of intermediate Macsyma code
; This function calls the Mma parser first.
(defun psm (fname)
  (init-translation)
  (to-macsyma-expr (psl fname)))

; input -- filename of file of Mma source
; output -- lisp string of Macsyma source code
; this calls all intermediates steps.
(defun pst(fname)
  (format t "~a" ( macsyma-source ( psm fname))))

; This the same as psl, but gives string output rather than list
; input -- filename of file of Mma source
; output -- prints stringified lisp list of intermediate Mma s-expression
(defun prsl(fname)
  (format t "~a" (psl fname)))

; This the same as psm, but gives string output rather than list
; input -- filename of file of Mma source
; output -- prints stringified lisp list of intermediate Macsyma code
(defun prsm(fname)
  (format t "~a" (psm fname)))

;; not sure if the line for NIL  here is useful
;; it prevents printing empty statements, which would cause errors.
(defun tfile (fname)
  (mapcar (lambda (expr) (cond 
;;   ((equal 'NIL expr)(format nil "~%"))
                          ((equal 'NIL expr)(format nil ""))
                          (t (macsyma-source-start expr)))) (psm fname)))

; read a file and translate it and print to stdout
(defun read-trans-print-file (fname)
;  (format t "Got file name in ptfile ~a~%" fname)
  (format t "~{~a~%~}" (tfile fname)))

; Parse a file of Mma code and return a string of
; maxima code.
(defun stfile (fname)
  (format nil "~{~a~%~}" (tfile fname)))

(defun write-translation (fname content)
  (with-open-file ( stream fname ;  this part is broken --> :external-format charset:iso-8859-1
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (format stream content))
  fname)



(defun add-declared-arrays (array-list)
  (setf array-list (rest array-list))
  (mapc (lambda (x) (increment-hash-value (maxima::concat x) *array-head-hash*))  array-list))

;; clear the hashes that try to keep track of Mma arrays
(defun init-translation ()
  ( cond ( (eq blank-sequence-var-args 'blank-sequence-val-nil) )
         ( t
           (format t "mma-to-max: init-translation blan-sequence-var-args not nil~%")
           (setq blank-sequence-var-args 'blank-sequence-val-nil ) ))
  (clrhash *array-element-hash*)
  (clrhash *array-head-hash*)
  (cond ( (and (boundp  'maxima::$mixima_translate_arrays) (listp maxima::$mixima_translate_arrays))
          (add-declared-arrays maxima::$mixima_translate_arrays))
        (t t)))


; have to find a way to put a string valued parameter in the hash.
; I can only put literals in.
;(defvar *sc-cr* (string (format nil ";~%")))
