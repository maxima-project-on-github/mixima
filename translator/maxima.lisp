
;;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mathematica(tm)-like evaluator
;; copyright (c) 2009 Richard J. Fateman
;; conversion from mockmma internal form to maxima and reverse.
;; missing: maxima special forms like mrat, taylor, pois, 
;; ignores maxima "noun" forms
;; maxima does not attribute any meaning to MockMMa patterns.
;; I'm not convinced I know what packages to put symbols. :maxima or :mma.

;;Programs defined  max2math math2max maxeval
;;  try, (tl)  MaxEval[Integrate[Sin[z]*z,z]]

;; RJF 6/30/09

(eval-when (compile) (load "mma"))
(in-package :mma)
(provide 'math2max)
(in-package :mma)
;; (eval-when (load) (export '(|Math2Max| |Max2Math| maxeval )))

(defun conc$(u &optional (pack :user))
  (cond ((numberp u) u)
	((symbolp u)(intern (concatenate 'string
			      "$"
			      (symbol-name u))
			    ;; intern in which package??
			    pack))))
(defun math2max(e)(|Math2Max| e))  ;; because GCL parser ignores case?
(defun max2math(e)(|Max2Math| e))
(defun maxeval(e)(|MaxEval| e))

(defun maxapply(op args)
  (maxima::mapply op args op))
       
(defvar math2max-verbose nil)
(defun |Math2Max|(e)
  (if (not (null  math2max-verbose))(format t "~%Math2Max: ~a" e ))
  
  ;  (setq e (car e))
  ;(cond  ((atom e) (format t "a:~a:a" (get e 'math2max))))
  
  (cond 
    ((atom e) 
     (cond((symbolp e) ;(format t "~%A symbol: ~a" e)
           (cond ((equal '|False| e) nil) 
             ((get e 'math2max)   );  if known conversion, return it.
             (t(conc$ e :maxima)
               ;;(conc$ e );; for testing
               )));; A -> $A 
       ;; convert numbers like 1/2
       ((cl:or (integerp e)(floatp e))e)
       ((stringp e) e)
       ((realp e)
        (list 
         ;;'(maxima::rat)
         '(maxima::rat)
         (numerator e)(denominator e)))
       (t (error "unimplement mathematica to maxima conversion of symbol ~s"e))))
    ;; a list
    ;((equal '|Times| (car e)) (cons '(maxima::MTIMES maxima::SIMP) (mapcar #'|Math2Max| (cdr e))))
    ;    ((equal '|Part| (car e)) (cons (list (|Math2Max| ( cadr e)) 'maxima::array) (mapcar #'|Math2Max| (cddr e))))
    ((equal '|Part| (car e))  (cons (list 'maxima::mqapply 'maxima::array) (mapcar #'|Math2Max| (cdr e))))
    ((equal '|Slot| (car e)) (cons '(maxima::$SLOT maxima::ARRAY) (mapcar #'|Math2Max| (cdr e))))
    ((equal '|Function| (car e)) (append '((maxima::LAMBDA)) '( ((maxima::MLIST) ((maxima::MLIST) maxima::$SLOT))) (mapcar #'|Math2Max| (cdr e))))
    ((equal '|Apply| (car e));(format t "~% Apply: ~a,~a" e (symbol-package (caadr e)))
     (append '( ( maxima::$APPLY)) (mapcar #'|Math2Max| (cdr e))))
    ;((equal '|List| (car e)) (list '( maxima::$copylist) (append '( ( maxima::mlist)) (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|BigFloat| (car e)) (list (list 'maxima::BIGFLOAT 'maxima::SIMP (cadddr e)) (cadr e) (caddr e)))
    ((equal '|Map| (car e))  (|Math2Max| (|Max2Math|(maxima::meval (cons '(maxima::$MAP) (mapcar #'|Math2Max| (cdr e))))))) ; Need to evaluate mapped function in Mockmma
    ;((equal '|Set| (car e))  (list '(maxima::msetq) (|Math2Max| ( cadr e))
    ;                                (maxima::mfuncall 'maxima::$copyifneeded (|Math2Max| (caddr e)))))
    ((equal '|Set| (car e)) (cons '(maxima::MSETQ) (mapcar #'|Math2Max| (cdr e))))
    ((equal '|Solve| (car e))  (cons '(maxima::$solve) (remove-is-from-solve (mapcar #'|Math2Max|  (cdr e)))))
    ((equal '|Sum| (car e))  (append '( maxima::$SUM) (cons (|Math2Max| (first (rest e))) (rest (car (mapcar #'|Math2Max| (cdr (cdr e))))))))
    ((equal '|Out| (car e)) (|Math2Max| (meval e)))
    ((equal '|SetDelayed| (car e)) (|Math2Max| (meval e)))
    ;((equal '|Equal| (car e)) (|Math2Max| (meval e)))
    ((equal '|Equal| (car e)) (list ' (maxima::$IS)   (append '( (maxima::$EQUAL))  (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|Increment| (car e)) (append '((maxima::MSETQ)) (mapcar #'|Math2Max| ( cdr e))  
                                          (list (append (list '(maxima::MPLUS) 1) (mapcar #'|Math2Max| ( cdr e))))))
    ((equal '|Decrement| (car e)) (append '((maxima::MSETQ)) (mapcar #'|Math2Max| ( cdr e))  
                                          (list (append (list '(maxima::MPLUS) -1) (mapcar #'|Math2Max| ( cdr e))))))
    ((equal '|Integrate| (car e)) (|Integrate| (mapcar #'|Math2Max| (cdr e))))
    ((equal '|If| (car e)) (|If| (mapcar #'|Math2Max| (cdr e))))
    ;((equal '|D| (car e)) (append '(maxima::mfuncall 'maxima::diff) (mapcar #'|Math2Max| (cdr e))))
    ((equal '|Less| (car e)) (list ' (maxima::$IS)   (append '( (maxima::MLESSP))  (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|Do| (car e)) (list ' (maxima::MDO) (|Math2Max| (car (cdaddr e))) 
                                 1 nil nil (|Math2Max| (cadr (cdaddr e))) nil (|Math2Max| (cadr e)) ))
    ((equal '|While| (car e)) (list ' (maxima::MDO) nil nil nil nil nil 
                                    (list '(maxima::MNOT) (|Math2Max| (cadr e))) (|Math2Max| (caddr e))))
    ((equal '|Greater| (car e)) (list ' (maxima::$IS)   (append '( (maxima::MGREATERP))  (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|GreaterEqual| (car e)) (list ' (maxima::$IS)   (append '( (maxima::MGEQP))  (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|LessEqual| (car e)) (list ' (maxima::$IS)   (append '( (maxima::MLEQP))  (mapcar #'|Math2Max| (cdr e)))))
    ((equal '|Real| (car e)) (append '(maxima::mplus 0.0) (mapcar #'|Math2Max| (cdr e))))
    ((msymbol-function (car e)) (meval e)) ; user defined function
    ((equal '|Table| (car e)) (append '(maxima::mfuncall 'maxima::|$Table|)
                                       (cons (list 'maxima::mquote (|Math2Max| ( cadr e))) (mapcar #'|Math2Max| (cddr e)))))
    ;    ((equal '|SetDelayed| (car e)) (cons '(maxima::MDEFINE maxima::SIMP) (mapcar #'|Math2Max| (cdr e))))
    (t(cons (list  (|Math2Max|  (car e)) ) (mapcar #'|Math2Max| (cdr e)))))) ; residual Maxima evaluation
;(t((|Math2Max| (meval e))))))
;(t((meval e)))))

;(setf (get '|Pattern| 'math2max) "")
;(setf (get '|Blank| 'math2max) NIL)
;(setf (get 'maxima::ifactors 'max2math) '|FACTORINTEGER|)

;(setf (get '|Set| 'math2max) 'maxima::msetq)
;(setf (get 'maxima::msetq 'max2math) '|Set|)


;(setf (get '|Apply| 'math2max) 'maxima::$apply)
(setf (get 'maxima::$apply 'max2math) '|Apply|)

(setf (get 'maxima::$SLOT 'max2math) '|Slot|)


(defun |Integrate| (args) (cond 
                            ((atom (car(last args))) (append '(maxima::$integrate) args ))
                            (t (append '( maxima::$integrate) (cons (|Math2Max|  (first args))   (mapcar #'|Math2Max|  (rest (car (cdr args)))))))
                            ))
(defun |If| (args) (append (append  (cons '( maxima::mcond) (reverse(cdr(reverse args)))) '(maxima::t)) (last args))) 
;(first args) (second args) ('maxima::t) (last args)s
;

(setf (get '|N| 'math2max) 'maxima::$mmamakebigfloat)
; Helper function to create bigfloats 
  (maxima::MEVAL '((maxima::MDEFINE) ((maxima::$MMAMAKEBIGFLOAT) ((maxima::MLIST) maxima::$FLOATARGS))
             ((maxima::MCOND) ((maxima::MEQUAL) ((maxima::$LENGTH) maxima::$FLOATARGS) 2)
              ((maxima::MPROG)
               ((maxima::MLIST) ((maxima::MSETQ) maxima::$FPPREC ((maxima::$FLOATARGS maxima::ARRAY) 2)))
               ((maxima::$BFLOAT) ((maxima::$FLOATARGS maxima::ARRAY) 1)))
              maxima::T ((maxima::$BFLOAT) ((maxima::$FLOATARGS maxima::ARRAY) 1)))))


(setf (get '|Log| 'math2max) 'maxima::$mmalog)
(setf (get 'maxima::%log 'max2math) '|Log|)
; Helper function to compute logs to other bases
;mmalog([x]):=if length(x)=1 then log(x[1]) else log(x[2])/log(x[1]);
 (maxima::MEVAL '((maxima::MDEFINE) ((maxima::$MMALOG) ((maxima::MLIST) maxima::$X))
             ((maxima::MCOND) ((maxima::MEQUAL) ((maxima::$LENGTH) maxima::$X) 1)
              ((maxima::%LOG) ((maxima::$X maxima::ARRAY) 1)) T
              ((maxima::MQUOTIENT) ((maxima::%LOG) ((maxima::$X maxima::ARRAY) 2))
               ((maxima::%LOG) ((maxima::$X maxima::ARRAY) 1))))))


(setf (get '|Random| 'math2max) 'maxima::$mmarandom)
(setf (get 'maxima::$mmarandom 'max2math) '|Random|)
; Helper function for random numbers
; mmarandom([x]):=if length(x)=0 then random(1.0) else random(x[1]);
  (maxima::MEVAL '((maxima::MDEFINE) ((maxima::$MMARANDOM) ((maxima::MLIST) maxima::$X))
             ((maxima::MCOND) ((maxima::MEQUAL) ((maxima::$LENGTH) maxima::$X) 0) ((maxima::$RANDOM) 1.0) T
              ((maxima::$RANDOM) ((maxima::$X maxima::ARRAY) 1)))))

(setf (get '|LCM| 'math2max) 'maxima::$mmalcm)
(setf (get 'maxima::$mmalcm 'max2math) '|LCM|)
; helper function for lcm
; mmalcm([x]):=block(load ("functs"),apply(lcm,x));
(maxima::MEVAL '((maxima::MDEFINE) ((maxima::$MMALCM) ((maxima::MLIST) maxima::$X))
             ((maxima::MPROG) ((maxima::$LOAD) "functs") ((maxima::$APPLY) maxima::$LCM maxima::$X))))


(setf (get 'maxima::mequal 'max2math) '|Rule|)


(setf (get '|Erf| 'math2max) 'maxima::%erf)
(setf (get 'maxima::%erf 'max2math) '|Erf|)

(setf (get '|Erfc| 'math2max) 'maxima::%erfc)
(setf (get 'maxima::%erfc 'max2math) '|Erfc|)

(setf (get '|LaplaceTransform| 'math2max) 'maxima::$laplace)
(setf (get 'maxima::$laplace 'max2math) '|LaplaceTransform|)

(setf (get '|InverseLaplaceTransform| 'math2max) 'maxima::$ilt)
(setf (get 'maxima::$ilt 'max2math) '|InverseLaplaceTransform|)

(setf (get '|Gamma| 'math2max) 'maxima::$gamma)
(setf (get 'maxima::$gamma 'max2math) '|Gamma|)

(setf (get '|BesselJ| 'math2max) 'maxima::$bessel_j)
(setf (get 'maxima::$bessel_j 'max2math) '|BesselJ|)

(setf (get '|BesselY| 'math2max) 'maxima::$bessel_y)
(setf (get 'maxima::$bessel_y 'max2math) '|BesselY|)

(setf (get '|BesselI| 'math2max) 'maxima::$bessel_i)
(setf (get 'maxima::$bessel_i 'max2math) '|BesselI|)

(setf (get '|BesselK| 'math2max) 'maxima::$bessel_k)
(setf (get 'maxima::$bessel_k 'max2math) '|BesselK|)

(setf (get '|Print| 'math2max) 'maxima::$print)
(setf (get 'maxima::$print 'max2math) '|Print|)

(setf (get '|Select| 'math2max) 'maxima::$sublist_indices)
(setf (get 'maxima::$sublist_indices 'max2math) '|Select|)

;(setf (get '|Solve| 'math2max) 'maxima::$solve)
;(setf (get 'maxima::$solve 'max2math) '|Solve|)

(defun remove-is-from-solve (expr)  ;(format t "~%cdar expr: ~a" (cdar expr)) 
  
  (cond 
    ((equal (caaar expr) 'maxima::$is)  (cons (solve-reformatter (car expr)) (cdr expr)))
    ((equal (caaar expr) 'maxima::mlist)  (cons  (cons '(maxima::mlist) (mapcar 'solve-reformatter  (cdar expr)) )(cdr expr)))
        (t expr)
    )
  )

(defun solve-reformatter (expr) ;(format t "~%expr: ~a" expr) 
   (cons '(maxima::mequal) (cdadr expr)) )

;(setf (get '|Eigenvalues| 'math2max) 'maxima::$eigenvalues)
;(setf (get 'maxima::$eigenvalues 'max2math) '|Eigenvalues|)

(setf (get '|PartitionsP| 'math2max) 'maxima::$num_partitions)
(setf (get 'maxima::$num_partitions 'max2math) '|PartitionsP|)

(setf (get '|LegendreQ| 'math2max) 'maxima::$legendre_q)
(setf (get 'maxima::$legendre_q 'max2math) '|LegendreQ|)

(setf (get '|LegendreP| 'math2max) 'maxima::$legendre_p)
(setf (get 'maxima::$legendre_p 'max2math) '|LegendreP|)

(setf (get '|Sort| 'math2max) 'maxima::$sort)
(setf (get 'maxima::$sort 'max2math) '|Sort|)

(setf (get '|Plot| 'math2max) 'maxima::$wxplot2d)
(setf (get 'maxima::$wxplot2d 'max2math) '|Plot|)

(setf (get '|Plot3D| 'math2max) 'maxima::$wxplot3d)
(setf (get 'maxima::$wxplot3d 'max2math) '|Plot3D|)

(setf (get '|ContourPlot| 'math2max) 'maxima::$wxcontour_plot)
(setf (get 'maxima::$wxcontour_plot 'max2math) '|ContourPlot|)

(setf (get '|CompoundExpression| 'math2max) 'maxima::mprog)
(setf (get '|Module| 'math2max) 'maxima::mprog)
(setf (get 'maxima::mprog 'max2math) '|Module|)

(setf (get '|Reverse| 'math2max) 'maxima::$reverse)
(setf (get 'maxima::$reverse 'max2math) '|Reverse|)

(setf (get '|And| 'math2max) 'maxima::mand)
(setf (get 'maxima::mand 'max2math) '|And|)

(setf (get '|Not| 'math2max) 'maxima::mnot)
(setf (get 'maxima::mnot 'max2math) '|Not|)

(setf (get '|Or| 'math2max) 'maxima::mor)
(setf (get 'maxima::mor 'max2math) '|Or|)

(setf (get '|FactorInteger| 'math2max) 'maxima::$ifactors)
(setf (get 'maxima::$ifactors 'max2math) '|FactorInteger|)

(setf (get '|Expand| 'math2max) 'maxima::$expand)
(setf (get 'maxima::$expand 'max2math) '|Expand|)

(setf (get '|Pi| 'math2max) 'maxima::$%pi)
(setf (get 'maxima::$%pi 'max2math) '|Pi|)

(setf (get '|PrimeQ| 'math2max) 'maxima::primep)
(setf (get 'maxima::primep 'max2math) '|PrimeQ|)

(setf (get '|IntegerQ| 'math2max) 'maxima::integerp)
(setf (get 'maxima::integerp 'max2math) '|IntegerQ|)

(setf (get '|E| 'math2max) 'maxima::$%e)
(setf (get 'maxima::$%e 'max2math) '|E|)

(setf (get '|I| 'math2max) 'maxima::$%i)
(setf (get 'maxima::$%i 'max2math) '|I|)

(setf (get '|Infinity| 'math2max) 'maxima::$inf)
(setf (get 'maxima::$inf 'max2math) '|Infinity|)

(setf (get '|Plus| 'math2max) 'maxima::mplus)
(setf (get 'maxima::mplus 'max2math) '|Plus|)

(setf (get '|List| 'math2max) 'maxima::mlist)
(setf (get 'maxima::mlist 'max2math) '|List|)

(setf (get '|Times| 'math2max) 'maxima::mtimes)
(setf (get 'maxima::mtimes 'max2math) '|Times|)

(setf (get '|Dot| 'math2max) 'maxima::mnctimes)
(setf (get 'maxima::mnctimes 'max2math) '|Dot|)

(setf (get '|Power| 'math2max) 'maxima::power)
(setf (get 'maxima::mexpt 'max2math) '|Power|)

(setf (get '|ArcCos| 'math2max) 'maxima::%acos)
(setf (get 'maxima::%acos 'max2math) '|ArcCos|)

(setf (get '|ArcCosh| 'math2max) 'maxima::%acosh)
(setf (get 'maxima::%acosh 'max2math) '|ArcCosh|)

(setf (get '|ArcCot| 'math2max) 'maxima::%acot)
(setf (get 'maxima::%acot 'max2math) '|ArcCot|)

(setf (get '|ArcCoth| 'math2max) 'maxima::%acoth)
(setf (get 'maxima::%acoth 'max2math) '|ArcCoth|)

(setf (get '|ArcCsc| 'math2max) 'maxima::%acsc)
(setf (get 'maxima::%acsc 'max2math) '|ArcCsc|)

(setf (get '|ArcCsch| 'math2max) 'maxima::%acsch)
(setf (get 'maxima::%acsch 'max2math) '|ArcCsch|)

(setf (get '|ArcSec| 'math2max) 'maxima::%asec)
(setf (get 'maxima::%asec 'max2math) '|ArcSec|)

(setf (get '|ArcSech| 'math2max) 'maxima::%asech)
(setf (get 'maxima::%asech 'max2math) '|ArcSech|)

(setf (get '|ArcSin| 'math2max) 'maxima::%asin)
(setf (get 'maxima::%asin 'max2math) '|ArcSin|)

(setf (get '|ArcSinh| 'math2max) 'maxima::%asinh)
(setf (get 'maxima::%asinh 'max2math) '|ArcSinh|)

(setf (get '|ArcTan| 'math2max) 'maxima::%atan)
(setf (get 'maxima::%atan 'max2math) '|ArcTan|)

(setf (get '|ArcTanh| 'math2max) 'maxima::%atanh)
(setf (get 'maxima::%atanh 'max2math) '|ArcTanh|)

(setf (get '|Cos| 'math2max) 'maxima::%cos)
(setf (get 'maxima::%cos 'max2math) '|Cos|)

(setf (get '|Cosh| 'math2max) 'maxima::%cosh)
(setf (get 'maxima::%cosh 'max2math) '|Cosh|)

(setf (get '|Cot| 'math2max) 'maxima::%cot)
(setf (get 'maxima::%cot 'max2math) '|Cot|)

(setf (get '|Coth| 'math2max) 'maxima::%coth)
(setf (get 'maxima::%coth 'max2math) '|Coth|)

(setf (get '|Csc| 'math2max) 'maxima::%csc)
(setf (get 'maxima::%csc 'max2math) '|Csc|)

(setf (get '|Csch| 'math2max) 'maxima::%csc)
(setf (get 'maxima::%csc 'max2math) '|Csch|)

(setf (get '|Sec| 'math2max) 'maxima::%sec)
(setf (get 'maxima::%sec 'max2math) '|Sec|)

(setf (get '|Sech| 'math2max) 'maxima::%sech)
(setf (get 'maxima::%sech 'max2math) '|Sech|)

(setf (get '|Sin| 'math2max) 'maxima::%sin)
(setf (get 'maxima::%sin 'max2math) '|Sin|)

(setf (get '|Sinh| 'math2max) 'maxima::%sinh)
(setf (get 'maxima::%sinh 'max2math) '|Sinh|)

(setf (get '|Tan| 'math2max) 'maxima::%tan)
(setf (get 'maxima::%tan 'max2math) '|Tan|)

(setf (get '|Tanh| 'math2max) 'maxima::%tanh)
(setf (get 'maxima::%tanh 'max2math) '|Tanh|)

(setf (get '|Exp| 'math2max) 'maxima::$exp)
(setf (get 'maxima::$exp 'max2math) '|Exp|)

(setf (get '|Abs| 'math2max) 'maxima::mabs)
(setf (get 'maxima::abs 'max2math) '|Abs|)

(setf (get '|True| 'math2max) 'maxima::t)
(setf (get 'maxima::t 'max2math) '|True|)

;(setf (get '|False| 'math2max) 'maxima::nil)
(setf (get 'maxima::nil 'max2math) '|False|)

(setf (get '|Set| 'math2max) 'maxima::set)
(setf (get 'maxima::set 'max2math) '|Set|)

(setf (get '|D| 'math2max) 'maxima::$diff)
(setf (get '|Dt| 'math2max) 'maxima::$diff)
(setf (get 'maxima::%del 'max2math) '|Dt|)
(setf (get 'maxima::$diff 'max2math) '|D|)
(setf (get 'maxima::%derivative 'max2math) '|D|)

(setf (get '|Factorial| 'math2max) 'maxima::mfactorial)
(setf (get 'maxima::mfactorial 'max2math) '|Factorial|)

(setf (get '|Factor| 'math2max) 'maxima::$factor)
(setf (get 'maxima::$factor 'max2math) '|Factor|)

(setf (get '|PLOT| 'math2max) 'maxima::plot2d)
(setf (get 'maxima::plot2d 'max2math) '|Plot|)

;(setf (get '|Part| 'math2max) 'maxima::$part)
(setf (get 'maxima::$part 'max2math) '|Part|)

(setf (get '|Sqrt| 'math2max) 'maxima::%sqrt)
(setf (get 'maxima::sqrt 'max2math) '|Sqrt|)

(setf (get '|Greater| 'math2max) 'maxima::mgreaterp)
(setf (get 'maxima::mgreaterp 'max2math) '|Greater|)

(setf (get '|Round| 'math2max) 'maxima::%round)
(setf (get 'maxima::%round 'max2math) '|Round|)

(setf (get '|Floor| 'math2max) 'maxima::$floor)
(setf (get 'maxima::$floor 'max2math) '|Floor|)

(setf (get '|Ceiling| 'math2max) 'maxima::$ceiling)
(setf (get 'maxima::$ceiling 'max2math) '|Ceiling|)

(setf (get '|Mod| 'math2max) 'maxima::$mod)
(setf (get 'maxima::$mod 'max2math) '|Mod|)

(setf (get '|Max| 'math2max) 'maxima::$max)
(setf (get 'maxima::$max 'max2math) '|Max|)

(setf (get '|Min| 'math2max) 'maxima::$min)
(setf (get 'maxima::$min 'max2math) '|Min|)

(setf (get '|Sign| 'math2max) 'maxima::$sign)
(setf (get 'maxima::$sign 'max2math) '|Sign|)

(setf (get 'maxima::$pos 'max2math) 1)
(setf (get 'maxima::$neg 'max2math) -1)
(setf (get 'maxima::$zero 'max2math) 0)

(setf (get '|Re| 'math2max) 'maxima::$realpart)
(setf (get 'maxima::$realpart 'max2math) '|Re|)

(setf (get '|Im| 'math2max) 'maxima::$imagpart)
(setf (get 'maxima::$imagpart 'max2math) '|Im|)

(setf (get '|Conjugate| 'math2max) 'maxima::$conjugate)
(setf (get 'maxima::$conjugate 'max2math) '|Conjugate|)

(setf (get '|GCD| 'math2max) 'maxima::$gcd)
(setf (get 'maxima::$gcd 'max2math) '|GCD|)

(setf (get '|KroneckerDelta| 'math2max) 'maxima::$kron_delta)
(setf (get 'maxima::$kron_delta 'max2math) '|KroneckerDelta|)

;(setf (get '|Table| 'math2max) 'maxima::|$Table|)
(setf (get 'maxima::|$Table| 'max2math) '|Table|)

(setf (get 'maxima::|$Partition| 'max2math) '|Partition|)

;; these guys will have to be in the right packages...  mplus in maxima package.
;; write a program to go through the hundreds of names that you want to deal with here.

;; 

;; lapeyre added case conversion line below
;; just a stop-gap. remove an initial $ if it is there. Dunno about initial %.
(defun stripdol(u &optional (pack :user))
;(format t "stripdol ~a" u)
  (cond ((numberp u) u)
	((symbolp u)
	 (let ((n (symbol-name u))) ;
           (setf n (maxima::maybe-invert-string-case n))
	   (if (member (aref n 0) '(#\$ #\%))(intern (subseq n 1) pack)
	     u)))
	(t (error "stripdol ~s" u))))

(defun |MaxEval|(e)
  (|Max2Math|(maxima::meval (|Math2Max| e))))  
  
(defun |Max2Math|(e)
  (if (not (null  math2max-verbose))   (format t "~%Max2Math: got input '~a'" e))
  (cond ((atom e) 
         (cond ( (symbolp e) 
                 (cond ((get e 'max2math)(get e 'max2math));  if known conversion, return it.
                       (t;;(conc$ e :maxima)
                        ;;probably use 
                                        ;(stripdol e :maxima)
                                        ;(maxima::maybe-invert-string-case (string
                        (stripdol e :mma);; for testing
                        )));; $A -> A 
               ;; convert numbers like 1/2
               ((cl:or (integerp e) (floatp e)) e)
               ((equal e "") e) ; wxplot2d
               ((stringp e ) e) 
               (t (error "not implemented mathematica-to-maxima conversion of symbol ~s"e))))
        ( (eq (caar e) 'maxima::rat)
          (/ (cadr e)(caddr e)));; 1/2 etc
  ;; need to ratdisrep mrat forms, 
        ((eq (caar e) 'maxima::mrat)
         (|Max2Math| (maxima::ratdisrep e)))
        ;; need to ratdisrep mrat forms, taylor forms, poisforms, 
        ;; do something about bigfloats
        ((eq (caar e) 'maxima::bigfloat)
         (list '|BigFloat| (cadr e) (caddr e) (caddar e))) 
        ;; a list
        (t  (cons (|Max2Math| (caar e))
                  (mapcar #'|Max2Math| (cdr e))))))


;; GCL read-table need this change maybe

#+ignore
(defun mread1(&optional (stream t))
  ;;  (format t "~% next char = ~s" (pc))
  (cond ((member (pc)'( #\space #\tab #\page) :test #'char=)
	 (rc)(mread1))  ;; fix - 2x bug
	((digit-char-p (pc));; next character is a digit 0-9
	 (collect-integer 
	  (char-to-int(read-char stream)) 10)) ;radix 10 default
	(t (or	;;(read-preserving-whitespace stream nil 'e-o-l)
	    (read stream nil 'e-o-l)
	      '|False|)
	   ;; nil reads as False
	   )))
;;; an attempt to get (read) to do both cases.

(defun _()(setf *readtable* #.(copy-readtable))) ;restore old readtable program

(loop for C across
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      do
      (set-macro-character 
       C
       #'(lambda(stream char)
	   (values (intern 
		    (format nil "~A~A" char 
			    (if (alphanumericp; terminate?
				 (peek-char nil stream #\% t))
				(read stream t nil t)
			      ""))
		    :mma)))
       t				; non-terminating macro character
       mma::mathrt))
