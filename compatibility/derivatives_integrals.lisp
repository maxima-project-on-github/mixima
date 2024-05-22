;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Derivatives and Integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Derivatives 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|> Function D |#
;; This calls Maxima diff to do derivatives
;; The first branch handles gradients eg: D(f(x,y,z),[[x,y,z]])
;; The second branch changes D(f(x,y),x,[y,2]) etc. into D(f(x,y),[x,1],[y,2])
;; and then calls diff on the flattened list (using append)
;; Derivative tensors are not implemented. But we should check the itensor
;; diff to see if it can be used.
(defmfun |$d| (expr &rest args &aux  newargs)
 (cond ( (and (mfuncall '$listp (first args)) (mfuncall '$listp (second (first args))))
	 (return-from |$d| (cons '(mlist) (mapcar #'(lambda (aa) 
		  (mfuncall '|$d| expr aa)) (rest (second (first args))))))))
 (setq newargs (apply #'append (mapcar #'(lambda (x) 
   (cond ( (atom x) (list x 1))
    (t (rest x)))) args)))
    (mapply '$diff (cons expr newargs) '$diff))

#|> Function Dt |#
;; This works (probably) if expr is just a single expression
;; Things like Dt(a*x+b,x) appear to work but need testing.
;; The algorithm is perhaps not great.
;; We use %diff to represent the derivatives, while Mma uses
;; Dt. In other situations Mma represents derivatives differently
(defmfun |$Dt| (expr &rest args &aux res)
  (setq res (mfuncall '$diff expr))
  (cond ( (eq args nil)
	  (mfuncall '$diff expr))
	( (eq (length args) 1)
	  (setq res (append (list '(mquotient) res (list '(%del) (first args)))))
	  (setq res (mfuncall '$dttodiff (mfuncall '$expand res)))))
  res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Symbolic Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|> Function Integrate |#
;; can do Integrate(f(x),x)
;;        Integrate(f(x,y),x,y)
;;        Integrate(f(x,y),[x,0,1],y) , etc.
;; The commented out code works, but the implementation following is perhaps
;; more clear (maybe not)

#|
(defmfun |$Integrate| (expr &rest args &aux res)
  (setq res expr)
  (dolist (item args)
    (setq res
     (cond ( ($mapatom item)
	     (mfuncall '$integrate res item))
	   (t
	    (mapply '$integrate (cons res (rest item)) '$integrate)))))
  res)
|#

;; The code above works too. It is in a way simpler than
;; the following. It should perhaps use lisp reduce. Need to make cleaner fold
;; for folding maxima funcs from lisp
(defmfun |$Integrate| (expr &rest args)
  (mfuncall '|$Fold| 'mixima-integrate-1d expr (cons '(mlist simp) args)))

; 1d with either  x or [x,a,b] for integration var.
(defun mixima-integrate-1d (expr s)
  (cond ( ($mapatom s)
	  (mfuncall '$integrate expr s))
	(t
	 (mapply '$integrate (cons expr (rest s)) '$integrate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Numerical Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mixima-nintegrate-option-hash  (make-hash-table :test #'equal ))
(defvar mixima-nintegrate-method-hash  (make-hash-table :test #'equal ))
(defvar mixima-nintegrate-rule-args)

; Need to put in a facility to save and restore these values
(defun romberg-set-option-args ( &aux pgoal agoal maxrecur minrecur epsrel epsabs )
  (declare (special $rombergit $rombergmin $rombergabs $rombergtol))
;  (setq args (reverse args))
  (setq pgoal (gethash '|$PrecisionGoal|  mixima-nintegrate-option-hash))
  (setq agoal (gethash '|$AccuracyGoal|   mixima-nintegrate-option-hash))
  (setq maxrecur (gethash '|$MaxRecursion|   mixima-nintegrate-option-hash))
  (setq minrecur (gethash '|$MinRecursion|   mixima-nintegrate-option-hash))
  (if (eq nil pgoal) (setq pgoal 10))
  (if (eq nil agoal) (setq agoal '$inf))
  (if (equal nil maxrecur) (setq maxrecur 11))
  (if (equal nil minrecur) (setq minrecur 0))
  (setq epsrel (if (eq '$inf pgoal) 0.0 (expt 10.0 (* -1 pgoal))))
  (setq epsabs (if (eq '$inf agoal) 0.0 (expt 10.0 (* -1 agoal))))
  (setq $rombergit maxrecur)
  (setq $rombergmin minrecur)
  (setq $rombergabs epsabs)
  (setq $rombergtol epsrel))

(defun quags-append-option-args (args &aux epsrel workarray epsabs pgoal agoal)
  (setq args (reverse args))

  (setq pgoal (gethash '|$PrecisionGoal|  mixima-nintegrate-option-hash))
  (setq agoal (gethash '|$AccuracyGoal|   mixima-nintegrate-option-hash))
  (if (eq nil pgoal) (setq pgoal 10))
  (if (eq nil agoal) (setq agoal '$inf))
  (setq epsrel (if (eq '$inf pgoal) 0.0 (expt 10.0 (* -1 pgoal))))
  (setq epsabs (if (eq '$inf agoal) 0.0 (expt 10.0 (* -1 agoal))))

  (setq args (cons (list '(mequal simp) '$epsrel epsrel) args))
  (setq args (cons (list '(mequal simp) '$epsabs epsabs) args))

  (setq workarray (gethash '|$WorkArray|  mixima-nintegrate-option-hash))
  (if (eq nil workarray) (setq workarray 400)) ; make this larger  than default
  (setq args (cons (list '(mequal simp) '$limit workarray) args))

  (setq args (reverse args)))

;; following is plenty ugly
;; quad_qags returns a list. here, we only return the
;; first number
(defun mquad_qags (expr args &aux res)
  (setq args (quags-append-option-args args))
  (setq res (mapply '$quad_qags (append (cons expr args)) '$quad_qags))
  (cond ( (mfuncall '$listp res)
;	  ($print res)
	  (cond ( (equal (first (last res)) 0)
;		  (format t "Success: Last element ~a Returning ~a~%" (first (last res)) (second res))
		  (second res))
		(t (format t " Fail Returning ~a~%" 'NIntegrateFail)
;		   (format t "Last element ~a~%" (first (last res)))
		   'NIntegrateFail)))
	(t 
	 ;(format t "quadg gives ~a~%" res)
	 nil)))

;  (if (mfuncall '$listp res) (second res) (cons '(mquad_qags) (rest res))))

(defun mquad_qag1 (expr args &aux res callargs)
  (setq callargs (if (eq (length args) 3) (append args '(1))  args))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t  'NIntegrateFail)))
	(t  nil)))
(defun mquad_qag2 (expr args &aux res callargs)
  (setq callargs (if (eq (length args) 3) (append args '(2))  args))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t  'NIntegrateFail)))
	(t  nil)))
(defun mquad_qag3 (expr args &aux res callargs)
  (setq callargs (if (eq (length args) 3) (append args '(3))  args))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t  'NIntegrateFail)))
	(t  nil)))
(defun mquad_qag4 (expr args &aux res callargs)
  (setq callargs (if (eq (length args) 3) (append args '(4))  args))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t  'NIntegrateFail)))
	(t  nil)))
(defun mquad_qag5 (expr args &aux res callargs)
  (setq callargs (if (eq (length args) 3) (append args '(5))  args))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t  'NIntegrateFail)))
	(t  nil)))

(defun mquad_qag6 (expr args &aux res callargs)
;  (format t "In qag6 ~a ~a~%" expr args)
  (setq callargs (if (eq (length args) 3) (append args '(6))
		   args))
  (setq callargs (quags-append-option-args callargs))
  (setq res (mapply '$quad_qag (cons expr callargs) '$quad_qag))
  (cond ( (mfuncall '$listp res)
	  (cond ( (equal (first (last res)) 0)
		  (second res))
		(t 
		 ;(format t " Fail Returning ~a~%" 'NIntegrateFail)
		   'NIntegrateFail)))
	(t 
	 nil)))

(defun mromberg (expr args &aux res)
  (romberg-set-option-args )
  (setq res (mapply '$romberg (append (cons expr args)) '$romberg))
  (cond ( (numberp res) res)
	(t 'NIntegrateFail)))

#|

  Only some singularities at the ends are detected. eg 
  singularies in double integrals, exprs like Sin(x)/x.

  Oscillatory not detected.

  Romberg fails ungracefully, so we cant try anything after it.

|#

(defmspec mixerrcatch (form)
  (let ((errcatch (cons bindlist loclist)) ($errormsg nil) ret)
    (if (null (setq ret (let (*mdebug* )
			  (errset (mevaln (cdr form)) lisperrprint))))
	(errlfun1 errcatch))
    (cons '(mlist) ret)))

(defun singtest (val x expr)
  (mfuncall 'mixerrcatch (list '($substitute)  val x expr)))

(defun mixima-nintegrate-automatic (expr args &aux res ivar alim blim sing-flag)
  (setq ivar (first args))
  (setq alim (second args)) 
  (setq blim (third args)) 
  (if (or (eq 1 (length (singtest alim ivar expr)))
	  (eq 1 (length (singtest blim ivar expr))))
      (setq sing-flag t))
;  (format t "In automatic ~a~%" exprargs)
  (cond ( (or (member '$inf args) (member '$minf args))
;	  (format t "NIntegrate: Infinite range: choosing qagi~%")
	  (setq args (quags-append-option-args args))
	  (setq res (second (mapply '$quad_qagi (cons expr args) '$quad_qagi))))
	( sing-flag 
	  (format t "NIntegrate: Singularity at end of interval, choosing qags~%")
	  (setq res (mquad_qags  expr args)))
	(t
;	 (format t "NIntegrate: Choosing default qag rule 1~%")
;	 (format t "romberg args expr ~a args ~a~%" expr args)
;	 (setq res (mquad_qags  expr args)) ; kinda works but is slow
;	 (setq res (mapply '$romberg (cons expr args) '$romberg)) ; works fastest but fails ungracefully
	 (setq res (mquad_qag1  expr args)) ;  kinda slow but maybe best
;	 (setq res (mquad_qag3  expr args)) ; slow
	 (cond  ( (eq 'NIntegrateFail res)
;t		  (format t "NIntegrate: qag1 failed, trying GaussKronrod (qag) rule 6~%")
		  (setq res (mquad_qag6  expr args))
		  (cond  ( (eq 'NIntegrateFail res)
;			   (format t "NIntegrate: qag 6 failed, trying qags~%")
			   (setq res (mquad_qags  expr args))))))))
  res)


(defun rulep (e)
  (if (and (listp e) (eq (mfuncall '$op e) '|$Rule|)) t nil))

;; filter argument list, pulling out Rules and putting them in a hash
;; The filtered argument list is returned
;; Add code that checks for which rules are allowed for each function (passed in a list maybe)
(defun mixima-collect-rules (hash args &aux nonrules)
  (setf mixima-nintegrate-rule-args  nil)
  (dolist (arg args)
    (cond ( (rulep arg)
;	    ($print "rule " arg)
	    (setf (gethash (second arg) hash) (third arg))
	    (setf mixima-nintegrate-rule-args (cons arg mixima-nintegrate-rule-args)))
	  (t
	   (setq nonrules (cons arg nonrules)))))
  (setq nonrules (reverse nonrules))
  (setq mixima-nintegrate-rule-args (reverse mixima-nintegrate-rule-args))
  nonrules)

;    (setq item (mfuncall '$mixima_minus_inf_to_minf item)) ; replace -inf -> minf

; put in a utility file
(defun mixima-add-keys-hash (hash pairs)
  (mapcar (lambda (pair) (setf (gethash (first pair) hash) (second pair))) pairs))

(mixima-add-keys-hash mixima-nintegrate-method-hash '(
  (|$Automatic| mixima-nintegrate-automatic)
  (|$NewtonCotesRule| mromberg)
  (|$RombergRule|     mromberg)
  (|$QagsRule|        mquad_qags)
  (|$DonckerWynnRule| mquad_qags)
  (|$Oscillatory|       mquad_qag6)
  (|$GaussKronrodRule6| mquad_qag6)
  (|$GaussKronrod|     mquad_qag1)
  (|$GaussKronrodRule| mquad_qag1)
  (|$GaussKronrodRule1| mquad_qag1)
  (|$GaussKronrodRule2| mquad_qag2)
  (|$GaussKronrodRule3| mquad_qag3)
  (|$GaussKronrodRule4| mquad_qag4)
  (|$GaussKronrodRule5| mquad_qag5)))


#|> Function NIntegrate |#
;; can do NIntegrate(f(x,y),[x,a,b],[y,c,d]) , etc.
;; two functions are needed only to reverse order of iterators
;; quad_qag and options need to be added.
(defmfun |$NIntegrate| (expr &rest inargs &aux args)
  (clrhash mixima-nintegrate-option-hash)
  (setq args (mixima-collect-rules mixima-nintegrate-option-hash inargs))
  (multiNIntegrate  expr inargs args))

;; currently handle multidimensional integrals by brute force
;; This is a 'fold' operation
;; upon failure return a quoted NIntegrate on everything except the
;; arguments that have already been integrated
(defun multiNIntegrate (expr inargs args &aux res doneargs)
  (declare (ignore inargs))
  (setq res expr)
  (dolist (item (reverse args)) ; eg item = [x,a,b]
    (setq doneargs (cons item doneargs))
;      (format t "multiNItegrate Doing one integratl~%")
    (setq res (apply #'%mixima-nintegrate (cons res (rest item))))
    (if (eq nil res) (setq res (cons '(|$NIntegrate|) 
	     (cons expr (append doneargs mixima-nintegrate-rule-args)))))
    )
  res)

;; do one integration
(defun %mixima-nintegrate (expr &rest args &aux method method-func res)
;  (format t "In mixima-integrate expr ~a args ~a  ~%" expr args)
  (setq method (gethash '|$Method|  mixima-nintegrate-option-hash))
  (if (eq nil method) (setq method '|$Automatic|))
  (setq method-func (gethash method  mixima-nintegrate-method-hash))
  (cond ( (eq nil method-func) (format t "Unknown method ~a in NIntegrate~%" method)
	  (return-from %mixima-nintegrate nil)))
;  (format t "call is ~a~%" (list method-func expr args))
  (setq args (rest (mfuncall '$mixima_minus_inf_to_minf (cons '(mlist) args))))
;  (format t "Method ~a method-func ~a~%" method method-func)
;  (format t "call is ~a~%" (list method-func expr args))
  (setq res (apply method-func (list expr args)))
;  (format t "here ~a~%" (cons '(|$NIntegrate|) (list expr (cons '(mlist) args))))
;  ($print (cons '(|$NIntegrate|) (list expr (cons '(mlist) args))))
;  (if (eq nil res) (cons '(|$NIntegrate|) (list expr (cons '(mlist) args)))
    res)

