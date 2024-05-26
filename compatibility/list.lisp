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

  Generalized Transpose function. And other list related functions 

  TODO: 
   * In rtest_miximacompat.mac are some examples of missing functionality
   * indices should be arrays not lists, for efficiency.
   * error checking should be cleaned up, added, improved.
   * use defmfun instead of defmspec
   * see comment with word 'convoluted' below.

|# 


;; We want to only call this from the macro multi-do (or maybe another macro)
;; Following is an example of using inc-inds
;;  (loop do 
;;        ....
;; 	   (if (inc-inds inds dims) (loop-finish)))
;; increment the indices inds. The upper limit of each index is given by
;; given by lims. Return nil except when the indices roll over,
;; in which case return t.
;; This is like a numeration, except each digit is in a different base.
;; the indices should all start at zero and will roll over when they reach
;; the limit. Ie, if you have two decimal digits, you should set them to
;; zero, give limits of (10 10) and then repeatedly increment them.
;; Upon calling inc-inds on (9 9) roll will be t.
(defun inc-inds (inds lims &aux (n (- (length inds) 1)) (roll nil) )
  (loop for i from 0 to n do
    (setf (elt inds i) (+ (elt inds i) 1))
    (cond ((eq (elt inds i) (elt lims i))
	   (cond ( (eq i n) (setq roll t)))
	   (loop for j from 0 to i do
		 (setf (elt inds j) 0)))
	  (t (loop-finish))))
  roll)

; create a list of n indices initialized to 0
(defun mixima-make-inds (n)
  (make-list n :initial-element 0))

;; multi-do -- nested loop with variable number of levels
;; Example with two levels:
;; (multi-do (j '(2 3)) (format t "~a, " j))
;; (0 0), (1 0), (0 1), (1 1), (0 2), (1 2)
;; Here j has a local binding. We may also exit the multi-do loop early via
;; explicit call to loop-finish within the body.
;; We can add ability to take initial value to this later, if needed
(defmacro multi-do (inds-lims &rest body)
  (let ( (n (gensym "n-"))
         (nlims (gensym "nlims-" ))
         (inds (first inds-lims))
         (lims (second inds-lims)))
    `(let (,n ,nlims)
       (setf ,nlims ,lims)
       (setf ,n (length ,nlims))
       (let ( (,inds (mixima-make-inds ,n)) )
         (loop do
               ,@body
               (if (inc-inds ,inds ,nlims) (loop-finish)))))))


#|> Function List |#
(defmix |$List| ( (args :oo) )
  (cons '(mlist simp) args))
  

;; this is crude and covers only the simplest use.
#|> Function Count |#
(defmix |$Count| (lis item )
  (count item lis))

; make string [1,2,3] from list (1 2 3)
(defun maxima-list-format (lis)
  (format nil "[~{~a~^,~}]" lis))

; Generalized transpose lis contains two args, arr, and perm. Like Mma Transpose.
; This is called by Transpose, if appropriate.
; arr -- the array to transpose
; perm -- a permutation of the topmost n levels to be permuted
(defmspec $gen_transpose (lis  &aux inds n lims perm arr dims olis)
  (cond ( (or (> (length lis) 3) (< (length lis) 2))
	 ( merror "Transpose called with ~a arguments; 1 or 2 arguments are expected."
		  (- (length lis) 1))))
  (setq arr  (meval* (second lis))) ; arr the tensor to be transposed
  (cond ( (eq (length lis) 2) ; if there is no second argument, do an ordinary transpose
	  (setq perm '(2 1)))  
	(t
	 (setq perm  (rest (meval* (third lis)))))) ; perm is a lisp list giving permutation of indices
  (cond ( (not (listp arr)) (merror "Non-list passed to Transpose")))
  (setq dims (nested-list-dimensions arr)) ; get list of dimensions of levels of arr
  (setq n (length perm))
  (cond ( (> n (length dims)) 
	 (merror "Permutation ~a is longer than the dimensions ~a of the array." 
		 (maxima-list-format perm) (maxima-list-format dims))))
; We only want the first n elements of dims, because arr may be deeper but user only
; wants to permute n levels.
; Following is convoluted way to take first n elements of dims.
  (setq dims (reverse (nthcdr (- (length dims) n ) (reverse dims))))
  (setq olis (make-nested (part-perm dims perm))) ; set up empty maxima nested list of desired dimensions
  (setq lims dims)
  (setf inds (make-list n :initial-element 0)) ; index set that will be looped over
  (loop do 
	(set-part olis (get-part arr inds) (part-perm inds perm))
	(cond ( (inc-inds inds lims) (loop-finish)))) ; inc-inds returns true on roll-over
  olis)

; helper function for $gen_transpose
; permute the elements of expr according to perm for Transpose
; expr is usually a list of numbers. perm is a permutation of
; numbers from 1 through n. A list with the elements of expr permuted
; according to perm are returned. expr is intended to be a list of
; indices to be permuted.
; permutation meaning is 'opposite' of description below. fix this doc.
; Note that 1 is subtracted because elt assumes first element is numbered 0.
; ex: (part-perm '(a b c d e f g h)  '(1 2 3 4 8 7 6 5)) --> (A B C D H G F E)
(defun part-perm (expr perm &aux n outex)
  (setq n (length perm))
  (setq outex (make-list n))
  (loop for i from 0 to (1- n) do
	(setf (elt outex (1- (elt perm i))) (elt expr i)))
  outex)

#|> Function Array |#
;; Note regarding the line below:
;; (setq element (mapply expr  (mapcar #'1+ inds)))
;; 1 is added to each ind because elt is 0-based, while mma lists are 1-based
;; However, in addition, if I do things like 
;;(setq element (cons '(mlist) inds)), then each element seems to be looking
;; at the current value of inds rather than containing a copy of the value of
;; inds at the time of its assignment. The 1+ evidently forces a copy to be made.
;; Note that Mma gives something different for
;; Array(f,[2,3],[0,4]) in Mma 3.0, but 8.0 agrees with what we have here.
;; dims can be either a single number or a list Array(f,n) or Array(f,[n1,n2...])
;; offsets can be: 1) not present, 2) a single number used for all dims, a 3) list of numbers
(defmix |$Array| (expr dims (offsets :o) (head :o) &aux  n  res )
  (setq dims (if (listp dims) (cdr dims) (cons dims nil))); either drop the mlist, or make single number into lisp list
  (setq n (length dims))
  (cond ( (not (null offsets))
          (setq offsets (if (listp offsets) (cdr offsets) (make-list n :initial-element offsets)))) ; make list if number was given
        (t (setq offsets (make-list n :initial-element 1))))
  (setq res (if head (make-nested-h head dims) (make-nested dims)))
  (multi-do (inds dims)
        (set-part res (mapply expr  (mapcar #'(lambda (x y) (+ x y)) inds offsets) expr) inds))
  res)

#|> Function ConstantArray |#
(defmix |$ConstantArray| (c spec)
  (mfuncall '|$Array| `( (lambda) ((mlist) ((mlist) x )) ((mquote) ,c) )  (list '(mquote) spec)))


#|> Function Tuples |#
;; not all implemented, eg.  Tuples([a,b],[2,2]);
(defmix |$Tuples| (lis (num :o) &aux olis head)
  (cond ( (null num)
	  (setq lis (rest lis))
	  (setq num (length lis))
	  (setq head (first (first lis)))
	  (setq lis (mapcar #'rest lis))
          (multi-do ( inds (reverse (mapcar #'length lis)))
                    (setq olis (cons (cons head (mapcar #'(lambda (x y) (nth x y)) (reverse inds) lis)) olis)))
	  (setq olis (cons '(mlist) (reverse olis)))
	  olis)
	(t
	 (setq head (first lis))
	 (setq lis (rest lis))
         (multi-do ( i1 (make-list num :initial-element (length lis)) )
                   (setq olis (cons (cons head (mapcar #'(lambda (x) (nth x lis)) (reverse i1))) olis)))
	 (setq olis (cons '(mlist) (reverse olis)))
	 olis)))


#|> Function Flatten |#
(defmix |$Flatten| ( (e :natom) (nmax :o) )
  (cond ((eq nmax nil) ($flatten e))
        ((or (specrepp e) (mapatom e)) e)
        (t (mcons-op-args (mop e) (nflattenl-op (margs e) (mop e) nmax  0 )))))


(defun nflattenl-op (e op nmax n)
  (mapcan #'(lambda (e)
	      (cond ((or (eq n nmax) (mapatom e) (not (alike1 (mop e) op)))
		     (list e))
		    (t (nflattenl-op (margs e) op nmax (1+ n)))))
	  e))

#|> Function Map |#
;; Using Map([x]) := apply('map,x) failed with plot2d or Series or something.
;; All shadowed functions should prbly be done this way.
;; evaluation not controlled well. we evaluate once and  $map does again
(defmix |$Map| (func form)
  (mapply '$map (list func form) '$map))

; Construct a nested list with with number of elements at each level given by
; elements of inds. The value of each element is nil, each list is prepended
; with '(mlist) to  make the structure a nested maxima list.
; ex:  (%i2) :lisp (setq $mm (make-nested '( 4 3 2 )))
;      (%i3)  Dimensions(mm)    ---> [4,3,2]
(defun make-nested ( inds &aux (n (first inds)) (olist nil) )
  (cond ((eq 1 (length inds))
	(setq olist (make-list n)))
	(t 
	 (loop for i from 0 to (- n 1) do
	       (setq olist (cons (make-nested (rest inds)) olist)))))
  (cons '(mlist) olist))

;; make-nested with different head
(defun make-nested-h ( head inds &aux (n (first inds)) (olist nil) )
  (cond ((eq 1 (length inds))
	(setq olist (make-list n)))
	(t 
	 (loop for i from 0 to (- n 1) do
	       (setq olist (cons (make-nested-h head (rest inds)) olist)))))
  (cons (list head 'simp) olist))


; Argument is a nested maxima list (regular lengths) and returns a list of the length of
; each level, with the top level first.
; Descends until the level of atoms is found.
;  (nested-list-dimensions (make-nested '( 1 1 2 5 4))) --> (1 1 2 5 4)
(defun nested-list-dimensions (lis &aux dims)
  (setq dims '())
  (cond ((atom lis) dims)
	(t (setq dims (cons (length (rest lis)) (nested-list-dimensions (first (rest lis)))))))
  dims)

;; Following functions for looping over nested lists
;; They are used by many Mma compatibility functions,
;; and should probably be in a separate file.

; Following two functions seem to work well
; like part(expr,i,j,k,...) gets nested part. but it must
; be usable as lvalue.
; In the following two funcs, we add 1 to i when using elt, because
; the zeroeth element is always '(mlist).
; Discounting 'mlist, the indices pick out elements with zero offset
;; ex: (get-part '((mlist) ((mlist) 1 2) ((mlist) 3 4 5) 6 7) '(1 2)) --> 5
(defun get-part (expr inds &aux (i (elt inds 0)))
  (cond ((eq 1 (length inds))
	 (cond ( (listp expr) (elt expr (+ 1 i)))
	       (t  (merror "get-part indices out of bounds"))))
	(t
	 (get-part (elt expr (+ 1 i)) (rest inds)))))

; args are  expr repl part-spec , part-spec is a list
; replace part in expr specified by list part-spec with repl
; indices are 0 based, taking (mlists) into account for macsyma lists
; ex: old (setq m '(mlist (mlist 1 2) (mlist 3 4 5) 6 7))
;     (set-part m 37 '(1 2))
;      m --> (mlist (mlist 1 2) (mlist 3 4 37) 6 7)
;; (setf m '( (mlist) ((mlist) 1 2) ((mlist) 3 4 5) 6 7))
;; ($disp m) --> [[1, 2], [3, 4, 5], 6, 7]
;; (get-part  m '(1 1)) --> 4
;; (set-part m '$CAT '(1 1)) --> $CAT
;; ($disp m) --> [[1, 2], [3, cat, 5], 6, 7]
(defun set-part (expr repl inds &aux (i (first inds)))
  (cond ((eq 1 (length inds))
	 (cond ( (listp expr) (setf (elt expr (+ 1 i)) repl))
	     (t  (merror "set-part indices out of bounds"))))
	(t
		(set-part (elt expr (+ 1 i)) repl (rest inds)))))
