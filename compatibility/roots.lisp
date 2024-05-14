(defvar mixima-findroot-option-hash  (make-hash-table :test #'equal ))
; should be factored out of the other two routines in NIntegrate that use this

(defun findroot-set-option-args ( &aux pgoal agoal epsabs epsrel maxits args)
  (setq pgoal (gethash '|$PrecisionGoal|  mixima-findroot-option-hash))
  (setq agoal (gethash '|$AccuracyGoal|   mixima-findroot-option-hash))
  (setq maxits (gethash '|$MaxIterations|   mixima-findroot-option-hash))
  (if (eq nil pgoal) (setq pgoal 10))
  (if (eq nil agoal) (setq agoal '$inf))
;  (format t "1hone~%" )
  (if (eq nil maxits) (setq maxits 1000))
  (setq epsrel (if (eq '$inf pgoal) 0.0 (expt 10.0 (* -1 pgoal))))
  (setq epsabs (if (eq '$inf agoal) 0.0 (expt 10.0 (* -1 agoal))))
  (setq $find_root_abs epsabs)
  (setq $find_root_rel epsrel)
  (setq args (list epsrel epsabs maxits))
 ; (format t "optargs ~a~%" args )
  args)


#|> Function FindRoot |#
(defmfun |$FindRoot| (expr &rest inargs &aux optargs eps args) 
  (clrhash mixima-findroot-option-hash)
  (setq args (mixima-collect-rules mixima-findroot-option-hash inargs)) ; filter rules from args
  (setq optargs (findroot-set-option-args))
  (setq args (rest (first args)))
;  (format t "length arga ~a,  ~a ~%"  (length args) args)
  (cond ( (eq (length args) 2)
	  (setq eps (second optargs))
	  (if (equal eps 0.0) (setq eps 0.000001))
;	  (format t "op is ~a ~a~%" ($op expr) expr)
	  (if (and (listp expr) (eq (first (first expr)) 'MEQUAL))
	      (setq expr (list '(mplus simp) (list '(mtimes simp) -1 (second expr)) (third expr))))
;	  (format t "new expr ~a~%"  expr)
;          ($print expr)
	  (list '(mlist) (list '(|$Rule|) (first args)
                (mapply '$%mixima_newton (list expr (first args) (second args) eps (third optargs)) '$%mixima_newton))))
	(t
	 (list '(mlist) (list '(|$Rule|) (first args) (mapply '$find_root (cons expr args) '$find_root))))))



  
