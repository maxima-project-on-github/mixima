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
|#


#|> Function VectorQ |#
;; VectorQ(v) gives true if v is a list, none of whose elements is a list and false otherwise.
;; VectorQ returns false for Macysma matrix objects.
(defmix |$VectorQ| (v)
  (and (mfuncall '$listp v) (not (some #'(lambda (x) (mfuncall '$listp x)) v))))

#|> Function MatrixQ |#
;; MatrixQ(m) gives true if m is a list, each of whose elements is a list
;; with the length of these elements being equal, and none of the elements of
;; these sublists is itself a list.
;; MatrixQ returns false for Macysma matrix objects
(defmfun |$MatrixQ| (m)
  (and (mfuncall '$listp m)
     (and  (every #'(lambda (x) (mfuncall '$listp x)) (cdr m))
	   (let ( (len (length (second m))))
	     (every #'(lambda (x) 
			(and (eq len (length x))
			     (notany #'(lambda (y) (mfuncall '$listp y)) x))) (cdr m))))))


;; Dimensions helper.
;; Return t if the macsyma expr m is non-atomic, and
;; has a certain head, and has a certain length.
(defun dimensions-check-list (m head len)
  (if (and (listp m) (eq head (mfuncall '$op m)) (eq len ($length m))) t nil))

;; Dimensions helper. Check if sub-lists at a given depth form a 'dimension'.
;; Input m is nested macsyma list m and dims a list of dimensions of some of the top levels.
;; Check elements of m at the depth given by the lenth of dims. If each element has the same head as
;; m and all elements have a common length, then return this length. Otherwise, return nil
;; signifying that they do not form a dimension. These elements are then the elements in
;; the deepest dimension and the list dims is complete.
(defun dimensions-level-check (m dims &aux inds len (levflag nil) (head (mfuncall '$op m)))
  (setf inds (mixima-make-inds (length dims)))
  (if  (atom (get-part m inds)) (return-from dimensions-level-check nil))
  (setq len ($length (get-part m inds)))
  (multi-do (i1 dims)
            (cond ((dimensions-check-list (get-part m i1) head len) nil)
                  (t (setq levflag  t) (loop-finish))))
  (if levflag nil len))

#|  good, but not using multi-do
(defun dimensions-level-check (m dims &aux inds len (levflag nil) (head (mfuncall '$op m)))
  (setf inds (mixima-make-inds (length dims)))
  (if  (atom (get-part m inds)) (return-from dimensions-level-check nil))
  (setq len ($length (get-part m inds)))
  (loop do 
	(cond ((dimensions-check-list (get-part m inds) head len) nil)
	      (t (setq levflag  t) (loop-finish)))
	(if (inc-inds inds dims) (loop-finish)))
  (if levflag nil len))
|#

#|> Function Dimensions |#
(defmix |$Dimensions| ( m  (maxdepth :o :non-neg-int) &aux dims nlev)
  (cond ( ($mapatom m) '((mlist)))
        ( t
          (setq dims (list ($length m)))
          (loop do
                (if (or (eq 0 nlev) (and maxdepth (eq maxdepth (length dims)))) (loop-finish))
                (setq nlev (dimensions-level-check m dims))
                (if nlev (setq dims (append dims (list nlev))) (loop-finish)))
          (cons '(mlist) dims))))

#|
(defmfun |$Dimensions| (m &optional maxdepth &aux dims (nlev nil))
  (setq m (meval m))
  (if ($mapatom m) (return-from |$Dimensions| '((mlist))))
  (setq dims (list ($length m)))
  (loop do
       (if (or (eq 0 nlev) (and maxdepth (eq maxdepth (length dims)))) (loop-finish))
       (setq nlev (dimensions-level-check m dims))
       (if nlev (setq dims (append dims (list nlev))) (loop-finish)))
  (cons '(mlist) dims))
|#

#|> Function ArrayDepth |#
(defmix |$ArrayDepth| (m)
  ($length (mfuncall '|$Dimensions| (list '(mquote) m) )))

; why does doing meval on t1 and t2 cause an error here, but not above in Dimensions ?
; Do a binary dot. This is called by nary dot below.
; This handles tensors of any dimension. Vectors are handled in a separate branch.
; Need to write a branch for matrices for efficiency
(defmfun $mixima_one_dot (t1 t2 &aux n j t3 dim1 dim2 dim3 resel)
  (cond ((and (mfuncall '|$VectorQ| t1) (mfuncall '|$VectorQ| t2))
	 (if (eq (length t1) (length t2)) nil
	   (progn  ($print "Vectors have different lengths")
		   (return-from $mixima_one_dot nil)))
	 (return-from $mixima_one_dot (meval (cons '(mplus)
		   (loop for i from 1 to (- (length t1) 1) collect  (list '(mtimes) (nth i t1) (nth i t2))))))))
  (setq dim1  (mfuncall '|$Dimensions| t1))
  (setq dim2  (mfuncall '|$Dimensions| t2))
  (cond ((not (eq  (first (last dim1)) (second dim2)))
	 ($print "Tensors have incompatible shapes")
	 (return-from $mixima_one_dot nil)))
  (setq n (second dim2))
  (setq j (- (length dim1) 2))
  (setq dim3 (append (subseq dim1 1 (- (length dim1) 1)) (subseq dim2 2 (length dim2) ))) ; append but leave contracted dimensions
  (setq t3 (make-nested dim3))
  (multi-do (inds dim3)
            (setq resel (meval (cons '(mplus)
                                     (loop for i from 0 to (1- n) collect
                                           (list '(mtimes) 
                                                 (get-part t1 (append (subseq inds 0 j) (list i)))
                                                 (get-part t2 (cons i (subseq inds (+ j 0) (- (length inds) 0)))))))))
            (set-part t3 resel inds))
  t3)

#|  good, but using multi-do
(defmfun $mixima_one_dot (t1 t2 &aux n j t3 dim1 dim2 dim3 inds resel)
  (cond ((and (mfuncall '|$VectorQ| t1) (mfuncall '|$VectorQ| t2))
	 (if (eq (length t1) (length t2)) nil
	   (progn  ($print "Vectors have different lengths")
		   (return-from $mixima_one_dot nil)))
	 (return-from $mixima_one_dot (meval (cons '(mplus)
		   (loop for i from 1 to (- (length t1) 1) collect  (list '(mtimes) (nth i t1) (nth i t2))))))))
  (setq dim1  (mfuncall '|$Dimensions| t1))
  (setq dim2  (mfuncall '|$Dimensions| t2))
  (cond ((not (eq  (first (last dim1)) (second dim2)))
	 ($print "Tensors have incompatible shapes")
	 (return-from $mixima_one_dot nil)))
  (setq n (second dim2))
  (setq j (- (length dim1) 2))
  (setq dim3 (append (subseq dim1 1 (- (length dim1) 1)) (subseq dim2 2 (length dim2) ))) ; append but leave contracted dimensions
  (setq t3 (make-nested dim3))
  (setf inds (mixima-make-inds (length dim3)))
  (loop do 
	(setq resel (meval (cons '(mplus)
          (loop for i from 0 to (1- n) collect
           (list '(mtimes) 
		 (get-part t1 (append (subseq inds 0 j) (list i)))
		 (get-part t2 (cons i (subseq inds (+ j 0) (- (length inds) 0)))))))))
	(set-part t3 resel inds)
	(if (inc-inds inds dim3) (loop-finish)))
  t3)
|#

#|> Function Dot |#
(defmspec |$Dot| (ls &aux res)
  (setq ls (cdr ls))
  (setq res ($mixima_one_dot (meval (first ls)) (meval (second ls))))
  (if (and res (> (length ls) 2))
      (loop for i from 2 to (1- (length ls)) do
	   (setq res ($mixima_one_dot res (meval (nth i ls))))
	   (if (eq res nil) (loop-finish))))
  res)
