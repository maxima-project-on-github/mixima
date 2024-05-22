(in-package :maxima)
#|

  VERSION VERSIONNUMBER

  Copyright (C) 2008, John Lapeyre  All rights reserved.

  This program is free software; you can redistribute it
  and/or modify it only under the terms of the GNU General
  Public License version 2 as published by the Free Software
  Foundation.

  Mathematica is a registered trademark of Wolfram Research,
  Inc. (2008)

  Sum and Product Mma compatibility functions.
  The functions $Sum and $Product call sum-or-product with a flag that says we want a sum or a product.
  Then sum-or-product calls $Sum or $Product recursively for each iterator and interprets each iterator.
  Finally it calls lsum,lproduct,dosum,miximadosum depending on the type of iterator and whether we want
  product or sum.
  ls contains (expr iter_1 iter_2 ...), where expr is the expression to sum over
  and iter_i are specifications of iterators over different variables. ie, if iter1 and
  iter2 are present, we have a double sum.
  If the step in an iterator is 1 (either explicitly or implicitly) then call the standard Macsyma
  dosum. If step != 1, then call our modified miximadosum.
  The argument op-plus in sum-or-product is t if we want a sum and nil if we want a product
|#


(defmspec |$Sum| (ls)
  (sum-or-product (cdr ls) t))

(defmspec |$Product| (ls)
  (sum-or-product (cdr ls) nil))

#| 

 The following reverse the order of the iterators, but I think Mma does not do this

(defmspec |$Sum| (ls)
  (setq ls (rest ls))
  (sum-or-product (cons (first ls) (reverse (rest ls))) t))
(defmspec |$Product| (ls)
  (setq ls (rest ls))
  (sum-or-product (cons (first ls) (reverse (rest ls))) nil))
|#

;; note we autoload simplify_sum here.
;; This code calls simplify_sum always on every product and sum, mostly useless.
(defun sum-or-product (ls op-plus &aux expr iter itvar  start stop result itlen recfun step)
; We choose to sum over last iterator (in list of iterators)
; first, and work inward. This choice matters since iterator
; limits of early iterators can depend on itvars of later
; iterators, but not vice-versa.  Since we call sum
; recursively, this means we start the last iteration first,
; ie the first iterator in the list of iterators.
  (when (< (length (mfuncall '$properties '$simplify_sum)) 3)
    (mfuncall '$load "simplify_sum"))
  (setq iter (cdr (second ls))) ; iter is now eg: '(i 1 10 2)  (cdr above is to drop '(mlist)
  (setq recfun (if op-plus '|$Sum| '|$Product|))
; call recursively, dropping the first iterator in the list of iterators
  (setq expr (if (> (length ls) 2) (apply 'mfuncall (cons recfun (cons (first ls) (cddr ls))))
        (first ls)))
  (setq itlen (length iter))
  (cond ((eq itlen 2)
	 (setq itvar (first iter))
	 (setq stop (second iter))
	 (setq start 1)
	 (setq step 1))
	((eq itlen 3)
	 (setq itvar (first iter))
	 (setq start (second iter))
	 (setq stop (third iter))
	 (setq step 1))
	((eq itlen 4)
	 (setq itvar (first iter))
	 (setq start (second iter))
	 (setq stop (third iter))
	 (setq step (meval (fourth iter)))))
  (if (symbolp itvar) nil (setq itvar (meval itvar))) ;if the iteration variable is not a symbol, hope it
                                                      ; evaluates to a symbol
; if stop is a list the user actually gave an explict list to iterate over.
  (setq stop (meval stop))
  (if (listp stop)
      (if op-plus
	  (setq result (mfuncall '$lsum expr itvar stop))
	(setq result (mfuncall '$lproduct expr itvar stop)))
    (setq result (if (eq step 1)
        (mfuncall '$simplify_sum 		     
 	 (dosum expr  itvar (meval start) (meval stop)  op-plus :evaluate-summand t))
	(miximadosum expr  itvar (meval start) (meval stop) step  op-plus :evaluate-summand t))))
  result)


; This function is modified from the Maxima function dosum in file asum.lisp
; The only change is to support stepsize. If the limit is not a fixed number
; return nil and print error message, because symbolic sums with step != 1 are
; not supported. (this function will not be called if step = 1)
(defun miximadosum (expr ind low hi step sump &key (evaluate-summand t))
  (declare (ignore evaluate-summand))
  (setq low (ratdisrep low) hi (ratdisrep hi)) ;; UGH, GAG WITH ME A SPOON
  (if (not (symbolp ind))
      (merror (intl:gettext "~:M: index must be a symbol; found ~M") (if sump '$sum '$product) ind))
  (unwind-protect
       (prog (u *i lind l*i *hl)
	  (setq lind (cons ind nil))
	  (cond
	    ((not (fixnump (setq *hl (mfuncall '$floor (m// (m- hi low) step)))));check if limit is a number, not a symbol
	     (format t "Symbolic sums and products with step != 1 not supported~%") (return nil))
;	     (if evaluate-summand (setq expr (mevalsumarg expr ind low hi)))
;	     (return (cons (if sump '(%sum) '(%product)) ; need to fix this stuff
;			   (list expr  ind low hi ))))
	    ((signp l *hl)
	     (return (if sump 0 1))))
	  (setq *i low l*i (list *i) u (if sump 0 1))
	  lo (setq u
		   (if sump
		       (add u (resimplify (let* ((foo (mbinding (lind l*i) (meval expr)))
						 (bar (subst-if-not-freeof *i ind foo)))
					    bar)))
		       (mul u (resimplify (let* ((foo (mbinding (lind l*i) (meval expr)))
						 (bar (subst-if-not-freeof *i ind foo)))
					    bar)))))
	  (when (zerop *hl) (return u))
	  (setq *hl (1- *hl))
	  (setq *i (car (rplaca l*i (m+ *i step))))
	  (go lo))))

; This is modeled on $lsum in asum.lisp
(defmspec $lproduct (l)
  (setq l (cdr l))
  (or (= (length l) 3) (wna-err '$lproduct))
  (let ((form (car l))
	(ind (cadr l))
	(lis (meval (caddr l)))
	(ans 1))
    (or (symbolp ind) (merror (intl:gettext "lproduct: second argument must be a variable; found ~M") ind))
    (cond (($listp lis)
	   (loop for v in (cdr lis)
	      with lind = (cons ind nil)
	      for w = (cons v nil)
	      do
	      (setq ans (mul* ans  (mbinding (lind w) (meval form)))))
	   ans)
	  (t `((%lproduct) ,form ,ind ,lis)))))
    
