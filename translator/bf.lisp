;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
(in-package :mma)
;;; Common Lisp Bigfloat Package, Part I: Basic arithmetic only.

;;  copyright (c) 1990    Richard Fateman, UC Berkeley


;;; Bigfloats are stored as structures: precision, fraction and exponent are
;;; integers, with an implied binary point to the left of the fraction. 
;;; The fraction is normalized (except for zero).

;;; We use the common lisp data structure type discrimination mechanism
;;; for tagging, printing, etc. bigfloats.

;;; Another file, bfelem provides sin/cos/log/exp of bigfloats.

;;; 
(provide 'bf)

(defvar bigfloat-bin-prec 10 
  "The number of bits in the fraction of a new bigfloat")

(defvar bigfloat-chop nil
"If T makes bigfloat arithmetic faster and less accurate by disabling rounding")  

(defvar bigfloat-print-trunc t 
  "If T means drop trailing 0's in the fraction when printing bigfloats")

(defvar bigfloat-printprec 0
  "If >0, then restrict printing to that many digits when printing bigfloats")

(proclaim 
  '(special bigfloatone bigfloatzero))

;; declare the structure of bigfloats

(defstruct (bigfloat 
	    (:constructor bcons2
			  (fraction exponent
				    &optional (precision bigfloat-bin-prec)))
	    (:print-function bigfloatprintfunction))
  (fraction  0                  :type integer :read-only t)
  (exponent  0                  :type integer :read-only t)
  (precision bigfloat-bin-prec  :type integer :read-only t)
  )

;; test for equality of two bigfloat

(defun bigfloat-eql(x y)
  (and (eql (bigfloat-fraction x)(bigfloat-fraction y))
       (eql (bigfloat-exponent x)(bigfloat-exponent y))))

;; how many bits does it take to represent x decimal digits?
(defun bits-in-decimal(x)
  (ceiling (* x (|Log| 10 2)))) ; deleted #.

;; initialization routines
;; Any time bigfloat precision is changed, one of these routine should
;; be called:  either bigfloat-init-dec or bigfloat-init-bin.

;; bigfloat-init-dec is
;; given: a certain number of decimal digits to be carried (approximately)

(defun bigfloat-init-dec (q)
  (bigfloat-init-bin (bits-in-decimal q)))

;; bigfloat-init-bin is
;; given: a certain number of bits to be carried in the fraction

(defun bigfloat-init-bin (q)  
  (setq bigfloat-bin-prec q
	bigfloatone (intofp 1) ;;initialize one
	bigfloatzero (bcons2 0 0)) ;; initialize zero
  q)

(defun intlen(x)(integer-length (|Abs| x)))
  
;; into-bigfloat is called by the reader to make a bigfloat.
;; The two arguments are integers representing frac * 10^exp.
;; The input routines should convert 1.23*10^5 to 123 * 10^(-2),
;; and then call into-bigfloat on 123 and -2.
;; the return value is an accurately converted number.

(defun into-bigfloat (frac exp)
  ;; log[2](10) is 3.321928 ...
   ;; jack up precision if it is needed for good conversion
   (let ((bigfloat-bin-prec 
	  (max bigfloat-bin-prec
	       (+ (ceiling 3.321193 (intlen exp))
		  (intlen frac)))))
     ;; compute frac*10^exp
     (bigfloat-* (intofp frac) (bigfloat-expt(intofp 10)exp))))

(defun bigfloat-convert(x) ;; takes whatever x is, and returns a bigfloat.
   (cond ((typep x 'bigfloat)
	  (cond((= bigfloat-bin-prec (bigfloat-precision x)) x)
	       (t (bfnormal 
		   (bigfloat-fraction x)
		   (bigfloat-exponent x)))))
	 ((typep x 'integer)(intofp x))  ;bignum, fixnum,
	 ;; this is the common-lisp rational number type
	 ((typep x 'ratio)
	  (bigfloat-/ ;; can't lose on under/overflow
	   (intofp (numerator x))
	   (intofp (denominator x))))
	 ;; for a flooating point number, convert to rational, first.
	 ((typep x 'float) ;single, double or other float
	  (bigfloat-convert (rational x)))
	 (t x))) ;; if it can't be converted, return x?
	     
;; integer to bigfloat conversion

(defun intofp(l)(if (equal 0 l) bigfloatzero (bfnormal l (intlen l))))

;; bfnormal "normalizes" a floating point fraction.
;; the proposed number has fraction frac (with binary bit at the left)
;; and exponent exp.
;; But frac may have the wrong number of bits.  To be
;; normalized, (intlen frac) must be = to bigfloat-bin-prec
;; unless frac =0.

(defun bfnormal(frac exp &optional (il (intlen frac)))
  (cond ((= il bigfloat-bin-prec) (bcons2 frac exp))
	(bigfloat-chop (bfnormal-chop frac exp il))
	(t(bfnormal-round frac exp il))))

(defun bfnormal-chop (frac exp il);
  (if (= frac 0) bigfloatzero
    (bcons2 (ash frac (- bigfloat-bin-prec il))
	    exp)))

(defun bfnormal-round (frac exp il);
  (if (= frac 0) bigfloatzero
    (let
	((s (- bigfloat-bin-prec  il)))
      (cond ((> s 0) ;; not enough bits, so shift left what you have
	     (bcons2 (ash frac s) exp))
	    ;; s= 0 taken care of in bfnormal.
	    (t  ;; that is, (< s 0) ;; too many bits. Must round.
	     (let ((r (round frac (ash 1 (- s)))))
	       (if (= (intlen r) bigfloat-bin-prec)
		   (bcons2 r exp)  ;; usual case
		 ;; Take care of rare case when rounding up bumps the length.
		 ;; Analogy in decimal: round 999 x 10^0 to 2 digits.
		 ;; You get 100 x 10^3.
		 (bcons2 (ash r -1) (1+ exp)))))))))

;; add two bigfloats of any precision; converts each to global precision.
;; c = a+b.

(defun bigfloat-+ (a b)
  (setq a (bigfloat-convert a))
  (setq b (bigfloat-convert b))
  (let* ((fa (bigfloat-fraction a))
	 (fb (bigfloat-fraction b))
	 (ea (bigfloat-exponent a))
	 (eb (bigfloat-exponent b))
	 fc
	 (dif (- ea eb)))
    (cond ((> dif     bigfloat-bin-prec) a) ;b is insignificant
	  ((> (- dif) bigfloat-bin-prec) b) ;a is insignificant
	  ((>= dif 0)
	   (setq fc (+ (ash fa dif) fb))
	   (bfnormal 
	    fc
	    (+ eb (- (intlen fc) bigfloat-bin-prec))))
	  (t (setq fc (+ (ash fb (- dif)) fa))
	     (bfnormal 
	      fc
	      (+ ea (- (intlen fc) bigfloat-bin-prec)))))))

;; bigfloat-* multiplies two bigfloats of arbitrary (perhaps different)
;; precisions, and return a bigfloat of global (bigfloat-bin-prec)
;; precision.

(defun bigfloat-* (a b) 
  (let* ((f (* (bigfloat-fraction a)(bigfloat-fraction b)))
	 (il (intlen f))
	 (exp   (+ (bigfloat-exponent a)
		   (bigfloat-exponent b)
		   (- (bigfloat-precision a))
		   (- (bigfloat-precision b))
		   il)))
    (bfnormal f exp il)))

;; bigfloat-/ divides two bigfloats of arbitrary (perhaps different)
;; precisions, and return a bigfloat of global (bigfloat-bin-prec)
;; precision.

(defun bigfloat-/ (a b) 
  ;; division by zero will be noticed in the "round" below
  (let* ((f (round (ash (bigfloat-fraction a)
			(+ bigfloat-bin-prec (bigfloat-precision b)))
		   (bigfloat-fraction b)))
	 (il (intlen f))
	 (exp   (+ (bigfloat-exponent a)
		   (- (bigfloat-exponent b))
		   (- (bigfloat-precision a))
		   (- bigfloat-bin-prec)
		   il)))
    (bfnormal f exp il)))

;; compute 1/x for bigfloat x

(defun bigfloat-inv(x)  (bigfloat-/ (bfone) x))

;; coerce-bigfloat is like the CL function coerce, but allows
;; the first argument to be of type bigfloat.  It converts the
;; first argument to  ratio, double-float, single-float (= float),
;; integer (= bignum or fixnum)

(defun coerce-bigfloat(x typ) 
  (cond ;; convert bigfloat to bigfloat of global precision
        ((eq typ 'bigfloat) 
	 (bigfloat-convert x))
	((eq typ 'ratio)
	 ;; convert a bigfloat to a rational fraction. This is exact in 
	 ;; the sense that for any represented bigfloat x in bigfloat-bin-prec,
	 ;; (bigfloat-convert( bigfloattorat x)) = x
	 (* (bigfloat-fraction x)
	    (expt 2 (- (bigfloat-exponent x) (bigfloat-precision x)))))
	((eq typ 'double-float)
	 ;; convert a bigfloat into a floating point double when it is
	 ;; simple to do.  Won't work for very high precision, since
	 ;; the fraction part will overflow.
	 (* (coerce (bigfloat-fraction x) 'double-float)
	    (expt 2.0d0 (-  (bigfloat-exponent x)(bigfloat-precision x)))))
	((member typ '(float single-float) :test #'eq)
	 (coerce (coerce-bigfloat x 'double-float) 'single-float))
	((member typ '(integer bignum fixnum))
	 (round (coerce-bigfloat x 'ratio)))
	(t (error "Can't coerce ~s to type ~s~%" x typ))))

;; how many decimal digits in an integer x?
;; slow, but accurate.  Used in i/o conversions.

(defun decimalsin(x)  (length (format nil "~s" x)))

;; various useful predicates

;; Seems like this function must exist in Maxima src/float.lisp or elsewhere,
;; but I couldn't find it. RHD 2024-05-18
(defun bigfloat-diff (a b)
  (maxima::bcons (maxima::fpdifference (rest a) (rest b))))

(defun bigfloat-> (a b) (bigfloat-posp (bigfloat-diff  a b))) 
(defun bigfloat-< (a b) (bigfloat-posp (bigfloat-diff  b a))) 
(defun bigfloat-posp (x)(> (bigfloat-fraction x) 0))
(defun bigfloat-zerop(x)(= (bigfloat-fraction x) 0))

(defun bigfloat-min(&rest args)
  (let (min)
    (cond ((null args) (error "bigfloat-min with no arguments")))
    (setq min (car args))
    (do ((a (cdr args)  (cdr a)))
	((null a) min)
	(cond ((bigfloat-< (car a) min)
	       (setq min (car a)))))))

(defun bigfloat-max(&rest args)
  (let (max)
    (cond ((null args) (error "bigfloat-max with no arguments")))
    (setq max (car args))
    (do ((a (cdr args)  (cdr a)))
	((null a) max)
	(cond ((bigfloat-> (car a) max)
	       (setq max (car a)))))))

;; return an existing 1 or make a new one.

(defun bfone nil 
       (cond
	((= bigfloat-bin-prec (bigfloat-precision bigfloatone)) 
	 bigfloatone)
	(t (setq bigfloatone (intofp 1)))))

;; compute x-y or -x  (if y is missing)

(defun bigfloat-- (x &optional (y nil))
  (cond (y (bigfloat-+ x (bigfloat-- y)));; compute x-y
	((bigfloat-zerop x) x)
	(t (bcons2 
	    (- (bigfloat-fraction x)) (bigfloat-exponent x)))))


;; compute p^n for bigfloat p.  nn is positive or negative integer.
;; Note that we do NOT allow bigfloat exponent here. (use log/exp for that)

(defun bigfloat-expt (p nn) 
  (cond ((eql nn 0) (bfone))
	((eql nn 1) p)
	((< nn 0) (bigfloat-inv (bigfloat-expt p (- nn))))
	(t 
	 (do ((n (floor nn 2)(floor n 2))
	      (s (cond ((oddp nn) p) (t (bfone)))))
	     ((zerop n) s)
	     (setq p (bigfloat-* p p))
	     (and (oddp n) (setq s (bigfloat-* s p))) )
	   )))

;; compute square-root of bigfloat x

(defun bigfloat-sqrt(x)(bigfloat-root x 2))

;; compute a^(1/n)  see Fitch, SIGSAM Bull Nov 74

(defun bigfloat-root (a n) 
  (let* ((ofprec bigfloat-bin-prec) 
	 (bigfloat-bin-prec (+ bigfloat-bin-prec 2))  ;assumes a>0 n>=2
	 (bk (bigfloat-expt (intofp 2)
			    (1+ (truncate (bigfloat-exponent
					   (setq a (bigfloat-convert a)))
					  n)))))
    (do ((x bk
	    (bigfloat-diff
	     x (setq bk (bigfloat-/ (bigfloat-diff
				     x 
				     (bigfloat-/ a (bigfloat-expt x n1)))
				    n))))
	 (n1 (1- n))
	 (n (intofp n)))
	((or (bigfloat-zerop bk)
	     (> (- (bigfloat-exponent x)
		   (bigfloat-exponent bk)) ofprec)) (setq a x))))
  (bigfloat-convert a)) ;return to previous precision
 
(defun bigfloat-abs (x) 
       (cond ((>= (bigfloat-fraction x) 0) x)
	     (t (bcons2 (- (bigfloat-fraction x)) (bigfloat-exponent x))))) 

;; integer part of a bigfloat

(defun bigfloat-intpart (f) 
 (let ((m (- (bigfloat-precision f) (bigfloat-exponent f))))
   (if (> m 0) (truncate (bigfloat-fraction f) (ash 1 m))
     (* (bigfloat-fraction f) (ash 1 (- m))))))


;;set up user's precision

;;(bigfloat-init-dec 3);; 3 decimal digits  for debugging

;; some tests, some utilities

(defun bc (x &optional
	     (bigfloat-bin-prec bigfloat-bin-prec )) (bigfloat-convert x))

;; this definition helps for tracing functions so you can read everything..
;; at least while you are using bignums that are exactly representable

(defvar bigfloat-print-trace nil "t if you want to see internals of bigfloats")

(defun bigfloatprintfunction (x s print-level)  ; print-level is not used.
  (declare (ignore print-level))
  (cond (bigfloat-print-trace
	 (format s "[~s*2^~s]=~a"
		 (bigfloat-fraction x)
		 (bigfloat-exponent x)
;;		 (coerce-bigfloat x 'double-float) ; test
		 (bigfloat-print x nil)
		 ))
	(t (bigfloat-print x s))))

;;(bigfloat-print x) normally prints the bigfloat to the standard output.
;;  it looks like d.dd*10^ee  where the number of digits, d, is specified
;;  by the width w, or if w is not provided, by the number of decimal
;;  digits assumed plausible by the binary precision.
;;  ee, the exponent, is computed as appropriate.

;;(bigfloat-print x stream) prints to a stream, and if stream=nil,
;; it returns a string.

;;(bigfloat-print x stream w) prints to a stream (if stream=t, standard
;; output; and also uses a width for the fraction part of w digits.

;; The total field width will be at most  w + 4 + width of exponent.
;; optionally there will be a negative sign taking one space, also.

;; this version of bigfloat-print prints d.ddd..d *10^n
;; except if n=0, when it prints d.ddd..d  (no *10^n).  Also
;; trailing zeros in the fraction are dropped off if bigfloat-print-trunc
;; is t, and 

(defun bigfloat-print (x &optional (stream t) (w bigfloat-printprec)&aux sign)
  ;(format t "~%bf print: ~a")
  (flet 
   ;; utility functions
   ((t0(s); change "12300" to "123"
       (if bigfloat-print-trunc (string-right-trim "0" s) s))
    (putindot(s) ; change "123" to "1.23"
	     (concatenate 'string (subseq s 0 1) "." (subseq s 1))))
   (cond((bigfloat-zerop x) (format stream "0.0"))
	(t (multiple-value-bind (frac exp)
				(bf-format x w)
				(setq sign (< frac 0))
				(setq frac (format nil "~s" (|Abs| frac)))
				(format stream "~:[~;-~]~a~:[*10^~s~;~]" 
					sign
					(t0(putindot frac))
					(zerop 
					 (setq exp(+ (length frac) -1 exp)))
					exp))))))


;; bf-format returns a possible pair of decimal fraction and exponent
;; values for printing x.
;; In particular, two integer values f and e are returned, and the
;; number x is closely approximated by f x 10^e.

(defun bf-format( x w)
  (cond ((bigfloat-zerop x) (values 0 0))
	(t
	 (let*((f (|Abs|(bigfloat-fraction x)))
	       (e (bigfloat-exponent x))
	       (p (bigfloat-precision x))
	       (bigfloat-bin-prec p)   ;use the precision of the number
	       (width (if (> w 0) w (truncate (* bigfloat-bin-prec 0.30103))))
	       ;; to convert .f  x 2^e to decimal
	       ;; compute the log[10] of the number, approximately
	       (d (floor
		   (+
		   ;; log of the exponent
		    (* (+ e (- p) (intlen f))
		      (/ 1.0d0(|Log| 10.0d0 2))) ;deleted #. which prevent maxima from loading
		   ;; log of the fraction
		   ;; we can do this in double-float since it is
		   ;; unreasonable to have an exponent exceeding +/- 10^307.
		   ;; Numbers of the size (2 ^(10^307)) won't
		   ;; print.
		   (|Log| (coerce (/ f (expt 2 (intlen  f)))
				'double-float)
			10))))
	       (s (if (>= d 0)
		      (bigfloat-/ x ;;note: x still has its sign
			      ;;this is cheap 
			      ;;(bigfloat-expt (intofp 10) d)
			      (intofp (expt 10 d));; this is accurate
			      )
		    (bigfloat-* x (intofp (expt 10 (- d))))))
	       ;; now  1 < |s| <= 10 ? , x = s * 10^d
	       (left(round (/(* (expt 10 (1- width))(bigfloat-fraction s))
				    (expt 2 (- (bigfloat-precision s)
					       (bigfloat-exponent s)))))) )
	   (cond ((> (decimalsin left) width)
		  (setq d (1+ d) left (round left 10))))	
	   ;; this could be set up as a string to return..
	   ;; The two items of interest that could be returned are
	   ;; the fraction and exponent

	   (values left (- d (1- width)))))))

(defun bigfloatformatter (args) 
  (bigfloat-print (bcons2 ( cadr args) (caddr args) (cadddr args)) nil))
