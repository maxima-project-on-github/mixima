;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-

;; convert from a list of lists
;; e.g. (Matrix (List a b)(List c d)) to a defstruct
;; note that arrays in CL are zero based, and so 
;; m [0,0] is the top left, and  m[1,1] is bottom right in
;;        ( a b )
;;  m  =  (     )
;;        ( c d )

(in-package :mma)

(defstruct matrix (a nil :type cl:array) (dimensions nil :type cl:cons))

(defun create-matrix (dims &optional (contents nil))  ;; a list of dimensions
  (if contents
      (make-matrix :a (make-array dims :initial-contents contents) :dimensions dims )
      (make-matrix :a (make-array dims) :dimensions dims )))

;; create a 2-d matrix by  Matrix[{a,b},{c,d}] etc.
(defun |Matrix|(&rest l)
  ;;should probably do some error checking 
  (let ((nrows(length l)) ;; (List a b) .. ignore List Head
	(ncols (length (cdar l))) ;; look at the first row for length
	(contents (mapcar #'cdr l))) ;; the content of the matrix
    (create-matrix (list nrows ncols) contents)))

;; convert from a 2-d rectangular dense array  in a defstruct to a list of lists
;; with head  "Matrix".  Mathematica would just have Head being List

(defun matrix2lists(a)
  (let ((ad (matrix-dimensions a)))
    (do ((res2 nil (ucons
		    ;; construct a row
		    (do ((res nil (ucons (aref (matrix-a a) i j) res))
			 ;;run through the row from right to left
			 (j (1- (cadr ad)) (1- j)))
			((< j 0)(ucons 'List res))
			;; empty do body
			)
		    res2))
	 ;; run through the matrix from bottom to top
	 (i (1- (car ad)) (1- i)))
	((< i 0)(ucons 'Matrix res2)) ;; or 'List for Mathematica
	;; empty do body
	)))

(setf (get 'matrix 'formatter) #'(lambda(x)(BuildFormat (matrix2lists x))))
    
(defun |CopyMatrix| (a)
  ;; assumes a is a matrix of some particular size
  ;; should be extended to copy a column, A[*,1] or a row A[1,*]
  (let (res
	(ad (matrix-dimensions a)))
    (setq res (create-matrix ad))
    (do ((i (1- (car ad)) (1- i)))
	((< i 0) res)
	 (do
	  ((j (1- (cadr ad)) (1- j)))
	  ((< j 0))
	  (setf (aref (matrix-a res) i j)(aref (matrix-a a) i j))))))
(defun
  |TransposeMatrix| (a)
  ;; assumes a is a matrix of some particular size
  (let (res
	(ad (reverse(matrix-dimensions a))))
    (setq res (create-matrix ad))
    (do ((i (1- (car ad)) (1- i)))
	((< i 0) res)
	 (do
	  ((j (1- (cadr ad)) (1- j)))
	  ((< j 0))
	  (setf (aref (matrix-a res) i j)(aref (matrix-a a) j i))))))
	    



