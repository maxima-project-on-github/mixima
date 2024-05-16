;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mathematica(tm)-like evaluator
;; copyright (c) 1990 Richard J. Fateman; pieces by Tak Yan, Derek Lai
;; just started converting atoms like Foo to |Foo|. 7/4/09 RJF   for GCL

(eval-when (:compile-toplevel) (load "mma"))
(in-package :mma)
;;(require "stack1") (require "disp1")(require "pf")(require "simp1")(require "rat1")(require "match")
(in-package :mma)
;(eval-when (:load-toplevel) (export '(mockmma mockmma2 mread1))
(eval-when (:load-toplevel) (export '(mockmma2 mread1)))

(defvar COUNT 1 "line number for top-level. (same as (meval $Line))")
(declaim (special env *expand*)) ;; environment
(declaim (special
	  |AddTo| |Alias| |And| |Apply| |Blank| |BlankNullSequence| |BlankSequence| 
	  |CompoundExpression| |Condition| |Delayed| |Derivative| |DivideBy| |Dot|
	  |Factorial| |Factorial2| |False| |Function| |Greater| |GreaterEqual| 
	  |If| |In| |Increment| |Inequality|
	  |Less| |LessEqual| |List| |Map| |MapAll| |MessageName|
	  |NonCommutativeMultiply| |Not| |Null| |Optional| |Or| |Out| |Part| |Pattern|
	  |PatternTest| |Pi| |Plus| |Power| |PreDecrement| |PreIncrement| ; |PrimeQ|
           |Put| |PutAppend|
	  |Real| |Repeated| |RepeatedNull| |Replace| |ReplaceAll| |ReplaceRepeated|
	  |Rule| |RuleDelayed| |SameQ| |Sequence|
	  |SetDelayed| |Slot| |SlotSequence| |SubtractBy| |TagSet|
	  |TagSetDelayed| |Times| |TimesBy| |True| |UnAlias| |Unequal| |UnSameQ| |UnSet|
	  |UpSet| |UpSetDelayed| 
	  |$Line| ))

;;      funnyvars is a hash table containing variables which, when set,
;;      cause function to be executed

(defvar funnyvars (make-hash-table :test #'eq :size 8))

(defvar mockmma-verbose t)
(defvar mockmmaeval nil)

;; in wxMaxima, use for instance :lisp (mockmma :mockmma-verbose t )

(defun mockmma  (&key mockmma-verbose mockmmaeval) ;; top level
  (let*
    ( (*package* (find-package :mma))
     (env (make-stack :size 50));environment for matching binding
     )
    (declare (special env *package*))
;    (maxima::mfuncall 'maxima::$load "mockmma/mockmma.mac")
        (if (not (null  mockmma-verbose)) (trace maxima::meval) ))        
  (if (= COUNT 1)
      (format t 
"Mockmma 1.0.0 (December 12, 2009) GNU General Public License
Mockmma uses Maxima to emulate a small subset of commands in the syntax of Mathematica (tm), 
which is a trademark of Wolfram Research Incorporated (WRI). 
Mockmma is in no way associated with WRI and is based on code written by Richard Fateman in 1990.")
      )
  (mockmma-read-eval-print-loop)
)

(defun mockmma-read-eval-print-loop ( &aux parsed-with-input )
  (let (h hs hin (timesofar 0) (mmaprefix "") (mmasuffix "") (timeunit (/ 1.0 internal-time-units-per-second)))
  (when (find-package :maxima)
    (setq mmaprefix maxima::*prompt-prefix*)
    (setq mmasuffix maxima::*prompt-suffix*))
  (loop
   (catch 'to-mockmma-repl
            (setf *debugger-hook* #'mockmma-debugger-repl)
          
          (setq timesofar (get-internal-run-time))
          (format t "~%~aIn[~s]:= ~a" mmaprefix COUNT mmasuffix)  ;; actually In and Out are variables too.
          (setq parsed-with-input (let ((maxima::errset nil)) (let ((foo (maxima::errset (mma::mma-parser)))) (if foo (first foo) (clear-input t)))))
;	  (format t "hin:  ~a~%" hin)
   ;(if (not (null  mockmma-verbose)) (format t "~%parsed raw:~a" hin))

   (setf hin (second parsed-with-input))             
   ; because wxMaxima appends a ";" to every input, ignore trailing ";" for the time being. hkm 7/16/09  
   (setq hin 
         (cond 
           ( (not (listp hin)) hin)
           ( (eql (car hin) '|CompoundExpression|)
            (cond
              ( (equal 3 (length hin))  (car (cdr (reverse hin))))
              (t(reverse (cdr (reverse hin))))
              ))
           ;(format t "~%num~a" (length hin)) (car (cdr (reverse hin)))); (nth 1 hin))
           (t hin)
           )
         )
   (if (not (null  mockmma-verbose)) (format t "~%parsed:~a" hin))        
   
   (|SetQQ| (ulist '|In| COUNT) hin)
   (setq h (let ((maxima::errset nil)) (let ((foo (maxima::errset (maxima::meval hin)))) (if foo (first foo) (list 'Hold hin)))))
   ;; (setq h (|MaxEval|  hin)) ;; <-- calling MaxEval is apparently an alternative to calling MEVAL; dunno how to choose one or the other
   
   (setq timesofar (- (get-internal-run-time) timesofar))
   ;; this is not the same as mathematica but I find it more convenient
   ;; this way. We've also implementing "Timing", if you prefer.
   (if (eq '|True| (meval '|$Showtime|))
       (format t "~%time = ~3,$ secs." (* timesofar timeunit)))
   
   (cond ((or (eql h '|Exit|)
              (and (listp h)(eq (car h) 'Quit)))
          (format t"~%Exited from Mockmma~%")
          (return t))
     (t	
      (|SetQQ| (ulist '|Out| COUNT) h)
      (cond((eq h '|Null|) nil)
        ;; don't print nil-valued answers
        (t
         (setq hs (list 
                   '|Set|
                   (ulist '|Out| COUNT) h))
         (if (not (null  mockmma-verbose))  (format t "~%Evaluated h: ~a" h))
         (if (not (null  mockmma-verbose))  (format t "~%Evaluated hs: ~a" hs))
         ;  	        (cond ((eql mmaprefix "<PROMPT-P/>") (dispwx COUNT h))
         ;  	          (T (disp (BuildFormat hs)))
         ;  	          )
         ;(dispwx COUNT h) ; Call this for wxMaxima
;	 (format t "hs is ~a~%" h)
;	 (format t "BF is ~a~%" (BuildFormat h))
         (disp (BuildFormat hs)) ; Call this for command line Maxima
         ))))
   (
    |Set|
    '|$Line| (setq COUNT (1+ COUNT)))            
   
   ))))


(defun mockmma-debugger-repl (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Mockmma encountered an error repl:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%")
  (throw 'maxima::to-maxima-repl t)
  )

(defun maxima::maxima-lisp-debugger (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Mockmma encountered an error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%")
  (throw 'mockmma2 t) ;  (throw 'maxima::to-maxima-repl t)
  )

;(defun maxima::maxima-lisp-debugger-repl (condition me-or-my-encapsulation)
;  (declare (ignore me-or-my-encapsulation))
;  (format t "~&Mockmma2 encountered an error repl:~%~% ~A" condition)
;  (format t "~&~%Automatically continuing.~%")
;  (throw 'to-mockmma-repl t))
;  )

;;    gjl tried to disabled this, but caused mixima to hang on loading. did not track it down
(defun maxima::maxima-lisp-debugger-repl (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Mockmma encountered a Lisp error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%To reenable the Lisp debugger set *debugger-hook* to nil.~%")
  (throw 'maxima::to-maxima-repl t)
  )

;; this definition replaces the program in the parser file

;;  gjl. did not notice this before. My modifications to mread1 in the parser code
;;  then have no effect. I need to integrate or fix this. a bunch of stuff in
;;  the translator. chash appears to be adding a hash table for each symbol to
;;  a hash table of all symbols. 
;;  Appears commenting out makes no change in rtests of mixima afterall, which
;;  is  expected.
#|
(defun mma::mread1()
  (cond((member (pc)'( #\space #\tab #\page) :test #'char=)
	(rc)(mma::mread1))
       ((digit-char-p (mma::pc));; next character is a digit 0-9
	(mma::collect-integer 
	  (mma::char-to-int(read-char stream)) 10)) ;radix 10 default
	;; for every alphabetic symbol, set up a hash table
       (t (or (chash(read-preserving-whitespace stream nil 'e-o-l) )
	      '|False|);; nil reads as False
	  )))

|#

(defun mread1( &aux savec savestr )
;    (format t "~% next char = ~s" (pc)) ;; debug
  (cond ((member (pc)'( #\space #\tab #\page) :test #'char=)
	 (rc)(mread1))  ;; fix - 2x bug
	((digit-char-p (pc));; next character is a digit 0-9
	 (collect-integer 
	  (char-to-int(rc)) 10)
	 )
				;radix 10 default
	(t
         (setf savec (chash(read-preserving-whitespace mstream nil 'e-o-l)))
         (cond  ( (equal savec 'e-o-l ) t)
            (t
             (cond ( (or (characterp savec) (stringp savec) (symbolp savec) (numberp savec) )
                     (setf savestr (string savec)))
                (t (setf savestr (format nil "~a" savec))))
            (setf input-string (concatenate 'string input-string savestr))))
         (or savec 'False) )))
	   ;; nil reads as False


;;enter a variable in the symbol table by making a hash
;; table as its value.

(defvar symtab (make-hash-table :test #'eq :size 150))

;; It's plausible to change this to use defstruct ... then
;; make every declared "symbol-table-entry" in lmath a structure
;; with (at least) the following data  (all now in same hashtable)
;;  (a) value for the symbol  e.g.  a=3
;;  (b) value for expressions with the symbol as head. e.g. a[45]=x+1
;;  (c) value for the collected attributes of the symbol.
;;       e.g. Attributes[a] ={Orderless, Protected, Listable}
;;  (d) value for each of the attributes 
;;  (e) value for function definition "built-in"  e.g. numeric cosine
;;  (f) value for user-defined function rules e.g. a[x_]:= ...
;;  (g) string for symbol printing (e.g. " + " for Plus)
;;  (h) formatting information (though could be on "Format")
;;  (i) left/right operator precedence information
;;  (j) messages/ documentation
;;  (k) package? context?

;;; possible types are (a): symbol; (b): hash-table; (c): list; (d) bit-vector;
;;; (e) lisp-function-value; (f) list? array? (g) string; (h) program?/
;;; (i)  (two) integers

(defun chash(m)
  (let ((*package* (find-package :mma)))
  (cond
   ((not(symbolp m))m)
;;   ((null m)nil) do we need to check for nil or t? Maybe not.
   (t (cond((gethash m symtab)) ;either it's there or
	   (t(setf (gethash m symtab)
		   (make-hash-table :test #'equal :size 4)); we make a hashtable

	     ))))
  m))

;; the following stuff is make-shift.

(defun |Head| (h)(if(consp h)(car h) (type-of h)))

;; the semantics are probably OK as long as only global properties
;; are being recorded.
;; SetAttribute puts info in two places so that the Attributes can
;; be gotten one at a time, and also collectively
;; define ClearAttribute similarly

(defun |SetAttribute|(h att &optional (val '|True|))
  (setq h (gethash h symtab))
  (setf (gethash att h) val)
   (setf(gethash '|Attributes| h)
		    (adjoin att (gethash '|Attributes| h)))
	(cons '|List| (gethash '|Attributes| h)))

(defun |Attributes|(h)
  `(|List| ,@(gethash '|Attributes| (gethash h symtab))))

;; Assignment statements treat the lhs with partial evaluation.
;; For a non-atomic Head, evaluate all the arguments, but not
;; the head. Presumably this should check attributes of the Head
;; like HoldAll, HoldFirst, etc. We don't do that yet.

;; we evaluate the lhs partially and then the rhs.
(defun holdallp(h) (member h '(|Timing| |SetQQ| |SetDelayed| |If| |Quote|)
			   :test #'eq)) ;for now

;; we'd like to have a Quote operator, but the repeated evaluation rule
;; makes it almost impossible to work unless we check for it specially..
;; alternatively, we can set *evalonce* to t, and (vastly)
;; change the semantics. Sometimes this vast change is no change at all....

(defun holdfirstp(h)(member h '(|Set|) :test #'eq)); for now

;; evaluate args, depending on the hold-specs of the head

(defun mevalargs( head l)
  (cond ((holdallp head) l)
       ((holdfirstp head)(ucons (car l)
				(umapcar #'meval (cdr l))))
       (t (umapcar #'meval l))))

;; note that the name of this function conflicts with that
;; of the lisp function set, unless
;; (a) capitalization is observed  OR
;; (b) the package system is protecting it..

(defun |Set| (lhs rhs &aux h fun);; lhs=rhs
  
  ;; the value associated with the lhs will be stored
  ;; in the symbol table symtab, with the key h,
  ;;  which is either the head of the lhs,
  ;; or the lhs itself.  That is  f=45 is stored in the
  ;; hash table for f, under the indicator "f"
  ;; and f[0]=7 is stored in the hash table for f, under
  ;; the indicator (0).
  
  (cond ((symbolp lhs)(setq h lhs))
	(t (setq lhs (mevalargs (setq h (car lhs))
				(cdr lhs)))))
  
  ;;(format t "Set ~s to ~s~%" h rhs)
  ;; this stores the material in the hash table.
  ;; QUESTION: M'ma doesn't do this, but we could, by storing
  ;; stuff on a local environment... f[x_,y_]:=Block[{},x=y+1];
  
  ;; what if (gethash h symtab) is a matrix, and this is a valid matrix
  ;; setting?  Then we should try to store the value in the array.
  ;; This is insufficient error checking but... 
    
  (if (and
       (symbolp h)
       ; is value of h an array? 
       (matrix-p (setq fun(gethash h (gethash h symtab))))
       ;; are subscripts integers in the right range?
       
       (every #'(lambda(n r) 
		  (and (integerp n) (> n 0)(<= n r)))
	      lhs
	      (matrix-dimensions fun))
       )
      ;; ok, then set the value in the array. Sorry it took so long.
      (setf (aref (matrix-a fun) (1- (car lhs)) (1- (cadr lhs)))
	     (setq rhs (meval rhs)))
    ;;else
		     
       (setf (gethash lhs  (gethash h symtab))
	(setq rhs (meval rhs))))
  ;; Next, check for special variables which, when set, cause other
  ;; things to happen. E.g. Precision= number means, in the
  ;; bigfloat system, (bigfloat-init number) gets executed.
  (if (setq h (gethash h funnyvars ))(funcall h rhs))
  rhs)

;; there is another file  (nmatrix) that defines a matrix type..
;; this should be tied in to the matrix stuff from franz, perhaps.
;; also, we have to decide which bigfloat to use... mpfun or rjf's
;; old bfstuff.

(defun matrix-p(x) (declare (ignore x)) nil) ;;; for now, this will have to do.
(defun |SetQQ|(lhs rhs &aux h);; lhs=rhs, but don't mevaluate either.
  
  (setq h(cond ((atom lhs) lhs)
	       (t (prog1 (car lhs) (setq lhs (cdr lhs))))))
  
  (setf (gethash lhs (gethash h symtab))rhs)
  
  (if (setq h (gethash h funnyvars ))(funcall h rhs))
  rhs)




(defun |SetDelayed|(lhs rhs) ;; this is the function definition f[x_] := ...
  (let* ((spot (gethash (|Head| lhs) symtab))
	 (rs (gethash '|SetDelayed| spot '|EmptyRuleset|))
	 k)
    (cond ((eq rs '|EmptyRuleset|) (setf (gethash '|SetDelayed| spot)
				       (list (cons lhs rhs))))
	  ;;check to see if lhs is equal to one of the other items already
	  ;;stored.
	  ((setq k(assoc lhs rs :test #'equal))
	   (cond((equal (cdr k) rhs)); just redefining same rule
		(t(format t "Replacing previous rule ~s ~%" lhs)
		  (rplacd k rhs)))
	   )
	  (t (push (cons lhs rhs) (gethash '|SetDelayed| spot))))
    '|True|))
;    '|Null|)) ; For testing need some return value 

;; this assumes the value of a mathematica symbol is its lisp value
;; if it is simply a constant or non-hash-table. That means that
;; a lisp dynamically bound variable could be used to block access
;; to the globally bound variable of the same name.  Better not
;; use local-variable names like Sin or Cos unless you mean to
;; inhibit access to the global names.

(defun meval-atom(h)  
  (declare (special env))
  (cond (env (setq h (sfind env h)))) 
  ;; if we find it here on the env stack, do we continue?
  ;; For now, we continue evaluating..
  (cond ((not(symbolp h)) h)
	(t(let ((r (gethash h symtab h)))
	    (cond
	     ((hash-table-p r)
	      (gethash h r h));; h is default if missing
	     (t r))))))			   


;; look up the value of e on e's hashtable, key e
;; look up the value of e[10] on e's hashtable, key (e 10)

(defun msymbol-value (h) 
  (let (tabentry)
    (cond
     ((and(symbolp h)(hash-table-p (setq tabentry(gethash h symtab h))))
      (gethash h tabentry))
     ((atom h) h) ;; an atom, not in the symbol table. Where did it come from?
     ;; must be a cons
     ((constantp (|Head| h)) h)
     ((setq tabentry(gethash (|Head| h) symtab))
      (gethash h tabentry h))
     (t h))))


(defun msymbol-function(h)
  (gethash '|SetDelayed| (gethash h symtab) nil)) ;; is this going to have the right scope?
;; 
;; must check for HoldAll, HoldFirst etc.
;;----end of makeshift definitions

(defun mapply (h args expr env);; note that expr == (h ,@args)
             (if (not (null  mockmma-verbose)) (format t "~%mapply:~a, ~a, ~a, ~a" h args expr env))        
  
  (let (fun)
    ;;get info on evaluating arguments and do it to args
    (setq args (mevalargs h args))
    (cond
     ;; I don't believe the comment below...
     ((constantp h) expr);; allows any lisp function, almost, to slip through
     ;; check for constant values pre-stored
     ((not (symbolp h)) (if (not (null  mockmma-verbose)) (format t "~%mapply ~a not symbolp" h)) ( setq expr(cons h args)))
     ((not (gethash h symtab)) (if (not (null  mockmma-verbose)) (format t "~%mapply ~a not gethash" h))(setq expr (cons h args)))
     ((multiple-value-bind 
       (val found)
       (gethash args (gethash h symtab))
       (if found (setq expr val) nil))t)
      
     ;; check for array...
     ((and 
	      (symbolp h)
	 (matrix-p (setq fun
		       (gethash h (gethash h symtab)))) ;; is value of h an array

       (every #'(lambda(n r) 
		  (and (> n 0)(<= n r)))
	      args
	      (matrix-dimensions fun)))
      (setq expr(aref (matrix-a fun) (1- (car args)) (1- (cadr args)))))
              
           
     ;; next check for user-defined  function
     ((setq fun (msymbol-function h)) (if (not (null  mockmma-verbose)) (format t "~%Next check for user-defined  function:~a" h ))
      (setq expr(rulesetapply h fun args)))
     ;; next check for built-in LISP function
     ;; (clearly not something that Mathematica does)
     ((and (symbolp h)(fboundp h))
      (if (not (null  mockmma-verbose)) (format t "~%Applying a lisp or other function ~s, args: ~s, mockmmaeval: ~s" h args (or mockmmaeval (eq h '|SetDelayed|))))
      (setq expr 
            (cond ((or mockmmaeval (eq h '|SetDelayed|) (eq h '|Equal|) (eq h '|Out|)) (apply h args))
              (t (|MaxEval| (cons h args)))
              )
            )) 
      ;(setq expr (apply h args))) 
           (t (if (not (null  mockmma-verbose)) (format t "~%mapply residual case: ~a" h ))(setq expr (|MaxEval| (cons h args)))) ; was (ucons h args)
      )
;;; what next?    

expr))

(declaim (special phead))

(defun rulesetapply(phead rules args)
    (declare (special isflat))
  ;; get attributes of phead and manipulate args
  ;; for now, assume evaluate all args, ignore Hold* attributes
    (if (not (null  mockmma-verbose))  (format t "~%Rule set apply: ~a, ~a, ~a" phead rules args))
    (if (not (null  mockmma-verbose))  (format t "~%Old env: ~a" env))
      (setq args (mevalargs phead args))
  (let* 
      ((ptr (stack-ptr env))
       (isflat (flatp phead))
       (isorderless (orderlessp phead))
       (origfn phead)
       res)
    (if isflat nil (setq phead '|Sequence|))

    (do ((r rules (cdr rules)) )
	((null r);; no more rules to try -- return original
	 (ucons origfn args))
	(let* ((thisrule (car r))
	      (condit t)
	      (lhs (car thisrule))
	      (rhs (cdr thisrule)))
	  ;; Note: if the rule was
	  ;; f[a_,b_]:= g[a,b] /; a>b, the parsed result is
	  ;; (SetDelayed (f ..) (Condition (g a b) (Greater a b)))
	  ;; see if there is a Condition on the rhs of the rule
	  ;; e.g. (Condition (foo a b) (Greater a b))
	  

	      (cond ((and (consp rhs)(eq (car rhs) '|Condition|))
		     (setq condit (caddr rhs))  ;condit = (Greater a b)
		     (setq rhs (cadr rhs)))) ;rhs = (foo a b)
	  (if (not (null  mockmma-verbose))  (format t "~%lhs= ~s, rhs= ~s, condition =~s env=~s" lhs rhs condit env))
	;; test for matching
	(cond ((cond 
		(isorderless (mlistol (cdaar r) args 0 (length args) condit))
		(t (mlist  (cdaar r) args nil condit)))
	       ;; if the appropriate matcher succeeded, then
	       ;; with the environment env from match, evaluate rhs.
	  (if (not (null  mockmma-verbose))  (format t "~%No2: lhs= ~s, rhs= ~s, condition =~s env=~s" lhs rhs condit env))
	       	       
                  (cond                   
	       	     (t (setq res (meval rhs))) ;evaluate using Mockmma 'mockmmaeval 
	       	     ;(t  (setq res (|MaxEval| rhs))) ;evaluate using Maxima
                                        )
	       ;; restore the environment
	       (setf (stack-ptr env) ptr)
	       ;; get out of the do-loop
	       (return res)))))))

;; Major evaluation function for an expression
;; see Mathematica book p 568
(defun meval-to-function(x) x) ;; don't evaluate function name 

(defvar *evalonce* nil)  ;; should be t to make quote (etc etc) work

(defun meval (e &aux res)
    (let ((saved e))
	  (if (not (null  mockmma-verbose))  (format t "~%Evaluating: ~s" e))
            (setq e
	    (cond((atom e)(meval-atom e))
		 ;; check off other constant expressions that don't evaluate.
		 ;; perhaps Messages?
		 ;;((patternobjectp e) e) .. What about Patterns?
		 ;; (mapply (car foo)(cdr foo)  foo) ==> foo  with no conses...
		 (t
		  (setq res
			(mapply (meval-to-function (|Head| e)) (cdr e) e env)))))
      ;; note the 3rd arg to mapply, just in case you want to
      ;; return the expression as the result without any change.
      ;; next step --
      ;;
      ;; do we keep evaluating until there is no change???
      (cond (*evalonce* e)
	    ((equal e saved)e)
;;
	      (t (meval e)))))


;; Each global object X is associated with a hash table
;; and we can, for each, 
;; to get the value, do (gethash X X), (gethash 'rules X) etc.

;; Local bindings hide everything.

;;Do we want to do infinite evaluation? 
;;How do we keep single copies of items in memory?
;;set up initial symbol-table entries for built-in stuff
;; should also set attributes

(mapc #'chash built-in-syms)

;; All system-level $-stuff can be initialized and stored this way
(defun globinit(name val)
  (chash name); just in case it isn't already there
  (setf (gethash name (gethash name symtab)) val))
  
(globinit '|$Line|	  1)
;(globinit '|False| nil)
(globinit '|I| #c(0 1)) ;; imaginary unit  I^2 = -1.

;; simple debugging tool: examine contents of symtable entry
(defun showhash(x)
  (maphash #'(lambda(key val) (format t "~%key=~s, val=~s" key val)) x))


; Attributes that evaluation routines use:
;    - Flattened [associative, flatten out nested expressions]
;    - Orderless [commutative, put args in order]
;    - HoldFirst [don't evaluate first arg yet]
;    - HoldRest [only evaluate first arg now]
;    - HoldAll [don't evaluate any args now]
;    - Procedure [procedure to call to do actual evaluation]
;    - Default

;[default value for optional variables]

(|SetAttribute| '|Plus| '|Flattened|)
(|SetAttribute| '|Plus| '|Orderless|)
(|SetAttribute| '|Plus| '|Default| 0)

(|SetAttribute| '|Times| '|Flattened|)
(|SetAttribute| '|Times| '|Orderless|)
(|SetAttribute| '|Times| '|Default| 1)

(|SetAttribute| '|Power| '|Default| 1)

(|SetAttribute| '|And| '|HoldAll|)         ; short-circuiting
(|SetAttribute| '|Or| '|HoldAll|)
(|SetAttribute| '|If| '|HoldRest|)
(|SetAttribute| '|Condition| '|HoldRest|)

(|SetAttribute| '|Set| '|HoldFirst|)
(|SetAttribute| '|SetDelayed| '|HoldAll|)
(|SetAttribute| '|UpSet| '|HoldFirst|)
(|SetAttribute| '|UpSetDelayed|	'|HoldAll|)
(|SetAttribute| '|TagSet| '|HoldFirst|)
(|SetAttribute| '|TagSetDelayed| '|HoldAll|)

(|SetAttribute| '|ReplaceAll| '|HoldFirst|)
(|SetAttribute| '|ReplaceRepeated| '|HoldRest|)

(|SetAttribute| '|Rule| '|HoldFirst|)
(|SetAttribute| '|RuleDelayed| '|HoldAll|)

;; convert all real numbers to exact rational numbers

(defun |Real|(a b)  (+ a b))

;; this works only for integer x,  x>0 
(defun decimalsin(x)
  (ceiling (* (integer-length x) 0.30102999566398114d0)))


;; handle %, %%, etc.

(defun |Out| (&rest n)
   (gethash (ucons
	  (cond ((null n) (1- COUNT))
		((minusp (setq n (car n))) (+ COUNT n))
		(t n))
	  nil)
	   (gethash '|Out| symtab)))
	 
(defun |Simp|(x)(simp x)) ;; rational simplification

(defun |Rat|(x)(into-rat x)) ;; leave the answer in rational form.
(defun |UnRat|(x)(outof-rat x)) ;; convert the answer to list form.

;; convert u to a rational with a single polynomial numerator
;; and denominator.  That is (x+1)^2 will be multiplied out.
;; result in rational form.


(defun |RatExpand|(u)
  (let* ((*expand* t) ;; global flag to rat program
	 (x (into-rat u)))
    (make-rat :numerator (make-fpe (fpe-expand (rat-numerator x))1)
	       :denominator
	       (make-fpe (fpe-expand (rat-denominator x)) 1))))

#+ignore (defun UnivariateDistinctDegreeFactorization (u)
  (let((x (into-rat u)))
    (make-rat :numerator   (poly-uddf  (rat-numerator x))
	      :denominator (poly-uddf  (rat-denominator x)))))
#+ignore (defun UnivariateSquareFree  etc)
#+ignore (defun UnivariateFactorizationMod(u p)
	   etc)

      

;; pick out parts of an expression.  x[[y]] parses to Part[x,y].
;; (a+r+b^c) [[3,2]] is c.
;;Generalizes somewhat in that (a+b^c)[[2,r]] returns (b^c)[[r]].
;; Does not handle negative part-numbers or lists of parts as
;; done in Mathematica. Also, won't decompose defstruct items
;; unless we do something about it for each structure...

(defun |Part|(u &rest k)(part1 u k))

(defun part1 (u kl)(cond ((null kl) u)
			 ((and(integerp (car kl))
			      (<= (length u)(car kl)))
			  (part1 (nth (car kl) u)(cdr kl)))
			 ;; leave unevaluated part if can't handle it.
			 (t (ucons '|Part| (ucons u kl)))))
			 
;; basic simplification of |Times|

;(defun |Times| (&rest x &aux (nums 1) oths)
; (dolist (h x   ;; iterate with h running over all args
;	   ;;resultform
;	   (cond((= 1 nums)
;		 (if (null oths) 1 
;		   (if (cdr oths) ;; more than one item in product 10/13/94
;		       (ucons '|Times| (uniq(nreverse oths)))
;		     (car oths))))
;		((null oths) nums)
;		(t (ucons '|Times| (ucons nums (uniq(nreverse oths)))))))
;	 ;; body
;	 (cond ((numberp h)(setq nums (* nums h))) ;; collect CL numbers
;	       ;; if you find a rat, break out !
;	       ((typep h 'rat)(return-from |Times|
;					   (reduce-rat #'rat* 
;						       (into-rat (car x))
;						       (cdr x))))
;	       (t (push h oths)))))


;; this definition allows  a=c; to take effect and return Null, so no display

(defun |CompoundExpression|(&rest x &aux ans)
  (do ((i x (cdr i)))
      ((null i) ans)  ;; evaluate each element in turn, return last.
      (setq ans  (meval (car i)))))

;(defun |Plus| (&rest x &aux (nums 0) oths)
; (dolist (h x   ;; iterate with h running over all args
;	   ;;resultform
;	   (cond((zerop nums)
;		 (if (null oths) 0 (ucons '|Plus| (uniq(nreverse oths)))))
;		((null oths) nums)
;		(t (ucons '|Plus| (ucons nums (uniq(nreverse oths)))))))
;	 ;; body
;	 (cond ((numberp h)(incf nums h))
;	       ;; if a rat form, break out!
;	       ((typep h 'rat)(return-from |Plus|
;					   (reduce-rat #'rat+ (into-rat (car x)) (cdr x))))
;	       (t (push h oths)))))


;(defun |Power| (b e)
;  (cond((and (numberp b)(numberp e))
;	(expt b e))
;       ((and (typep b 'rat)(integerp e))
;	(into-rat `(|Power| ,b ,e)))
;       (t (ulist '|Power| b e))))


;; note: Timing is a HoldAll function... otherwise the evaluation
;; of the argument would come first, and the timing would
;; be of an already evaluated expression.

(defun |Timing|(x) ; x is an expression, perhaps compound, uneval'd
  
  (let*((timeunit (/ 1.0 internal-time-units-per-second))
	(timesofar (get-internal-run-time))
	(result (meval x)))
    `(List (|Times| , (*(- (get-internal-run-time) timesofar)
		      timeunit)
		    Second) , result)))

;; if we are stuck with all one case
;; then we are forced to do something like this for
;; every function that is already defined in lisp with the
;; same name as in mathematica (tm)

(shadow '(sin cos tan log exp sinh cosh tanh abs sqrt
	  plusp) :mma) ;;etc

(defun |Sin|(x)(cond ((numberp x)(cl:sin x))(t (ulist '|Sin| x))))
(defun |Cos|(x)(cond ((numberp x)(cl:cos x))(t (ulist '|Cos| x))))
(defun |Tan|(x)(cond ((numberp x)(cl:tan x))(t (ulist '|Tan| x))))
(defun |Log|(x &optional b)(cond ((numberp x)
                                  (cond 
                                    ( b   (/ ( cl:log x) ( cl:log b)))
                                    ( t   (cl:log x))
                                    )
                                  )(t (ulist '|Log| x))))
(defun |Exp|(x)(cond ((numberp x)(cl:exp x))(t (ulist '|Exp| x))))

(defun |Sinh|(x)(cond ((numberp x)(cl:sinh x))(t (ulist '|Sinh| x))))
(defun |Cosh|(x)(cond ((numberp x)(cl:cosh x))(t (ulist '|Cosh| x))))
(defun |Tanh|(x)(cond ((numberp x)(cl:tanh x))(t (ulist '|Tanh| x))))

(defun |Abs|(x)(cond ((numberp x)(cl:abs x))(t (ulist '|Abs| x))))
;(defun |Sqrt|(x)(cond ((numberp x)(cl:sqrt x))(t (ulist '|Sqrt| x))))
;;(defun plusp(x)(cond ((numberp x)(cl:plusp x))(t (ulist 'plusp x))))
;; hmmm, what to do with "and"??
;; if the user types in And[a,b] then it becomes (and a b) 
;; which is plausible lisp, and there is no quick check.
;; dammt!  A user could type in a&&b  and be safer.

(if (find-package :maxima) t (defpackage :maxima))
;(export 'mma::mockmma)
(in-package :maxima)
(import 'mma::mockmma)

