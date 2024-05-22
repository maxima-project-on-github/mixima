#|

  Argument checking and manipulation for compatibility functions.
  We have some of the most basic behavior. Much more could be done.

|#

;; We want to use this function to process arguments to compatibility functions.
;; mixima-narg-check is quickly converted, but needs to be cleaned up.
;; call is the entire maxima call SomeFunc(a,b,d)
;; arg-spec is a specification of how those args should be checked and processed
(defun mixima-check-args (call arg-spec )
  (cond ( (and (getf arg-spec :nargs-spec) (not (mixima-narg-check call arg-spec))) nil )
        ( (and (getf arg-spec :arg-tests) (not (mixima-arg-tests call arg-spec))) nil )
        (t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *defmix-arg-check-table*  (make-hash-table))

(mma::set-hash-elements *defmix-arg-check-table*
     '(
       ( :natom mixima-not-mapatom-test )
       ( :pos-spec mixima-pos-spec-test )
       ( :non-neg-int mixima-non-neg-int-test )))

(defun mixima-arg-tests (call arg-spec &aux num res (checkf t) )
  (dolist (one-test (getf arg-spec :arg-tests))
    (setf num (first one-test))
    (cond ( (> (length call) num) ; only check arg if it is present
            (setf res (funcall (gethash (second one-test) *defmix-arg-check-table*)
                               call num (nth num call)))
            (if (not res) (setf checkf nil)))
          (t t)))
  checkf)

(defun mix-error-message-1 (type mssg call pos)
  (format t "~a::~a ~a expected at position ~a in ~a.~%"
                  (stripdollar (first (first call))) type mssg pos  ($sconcat call)))

(defun mixima-not-mapatom-test (call pos e)
  (cond ( ($mapatom e)
          (mix-error-message-1 "normal" " Nonatomic expression" call pos )
          nil)
        (t t)))

(defun mixima-non-neg-int-test (call pos e)
  (cond ( (or (and (integerp e) (>= e 0)) (eq '$inf e)) t)
        ( t
          (mix-error-message-1 "innf" "Non-negative integer or Infinity" call pos )
          nil)))

(defun mixima-pos-spec-test (call pos e)
  (declare (ignore pos))
  (cond ( (mixima-position-spec-p e) t)
        (t
         (format t "~a::psl: Position specification ~a in ~a~%    is not an integer or a list of integers.~%"
                        (stripdollar (first (first call))) ($sconcat e) ($sconcat call) )
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Check the number of arguments to a function call
;; print error argr (argrx) for too few (many) arguments.
;; Return nil if the number of args is incorrect, otherwise t.
;; This function looks strange now because it is being converted to use mixim-process-args
;; need to improve error message of nargs-spec is (min max)
(defun mixima-narg-check ( call arg-spec  &aux pname nargs-got nargs-spec nargs-expected )
  (setf nargs-spec (getf arg-spec :nargs-spec))
  (setf name (first (first call)))
  (setf nargs-got (length (rest call)))
  (if (and (listp nargs-spec) (eq (first nargs-spec) (second nargs-spec)))
      (setf nargs-spec (first nargs-spec)))
  (cond ( (listp nargs-spec)   ; range of args required
          (let ( (min (first nargs-spec)) (max (second nargs-spec)) )
            (cond ( (or (< nargs-got min) (> nargs-got max))
                    (setq pname (stripdollar name))
                    (format t "~a::argt~:[o~;u~] ~a called with ~a argument~:p; between ~a and ~a arguments are expected.~%"
                            pname (> nargs-got max) pname nargs-got min max )
                    nil)
                  (t t))))
        (t             ; fixed number of args required
         (setf nargs-expected nargs-spec)
         (cond ( (/= nargs-expected nargs-got)
                 (setq pname (stripdollar name))
                 (format t "~a::argr~:[~;x~] ~a called with ~a argument~:p; ~a argument~:p ~:[are~;is~] expected.~%"
                         pname (> nargs-got nargs-expected) pname nargs-got nargs-expected (\= 1 nargs-expected))
                 nil)
               (t t)))))


;; Check if position spec is valid.
;; This seems to be all that Mma does.
(defun mixima-position-spec-p (spec)
  (cond ( (or ($listp spec) (integerp spec))
          t)
        (t nil)))
  
;; normalize positiion specification to mlist of mlist of integers
;; Each specification is an mlist.  If a single mlist is found or
;; a single integer, coerce into mlist of mlists.
;; Ie  [ [ 1,1,1 ], [ 2,2,2 ] ]  is unchanged
;;     [ 1,1,1 ]  ->  [ [ 1,1,1 ] ]
;;         1  ->  [ [ 1 ] ]
(defun mixima-normalize-position-spec (spec)
  (cond ( ($listp spec)
          (cond ( ($listp (second spec))
                  spec)
                (t
                 (list '(mlist simp) spec))))
        ( (integerp spec)
          (list '(mlist simp) (list '(mlist simp) spec)))))
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                
;; UNUSED!!
#|
(defun mixima-psl ( call arg-spec newargs  &aux pname epos-spec pos-spec pos-spec-n )
  (setf pos-spec-n (getf arg-spec :pos-spec-n  ))
  ( cond (  pos-spec-n
            (setf pos-spec (nth pos-spec-n call))
            (setf name (first (first call)))
            (setq epos-spec (meval pos-spec))
            (cond ( ($listp epos-spec)
                    (cond ( ($listp (second epos-spec))
                            (setf (getf newargs ':pos-spec) epos-spec)
                            newargs)
                          ( t
                            (setf (getf newargs ':pos-spec) (list '(mlist simp) epos-spec))
                            newargs)))
                  ( (integerp epos-spec)
                    (setf (getf newargs ':pos-spec) (list '(mlist simp) (list '(mlist simp) epos-spec)))
                    newargs)
                  (t
                   (setq pname (stripdollar name))
                   (setq spos-spec ($sconcat epos-spec))
                   (setq scall ($sconcat call))
                   (format t "~a::psl: Position specification ~a in ~a~%    is not an integer or a list of integers.~%"
                           pname spos-spec scall )
                   nil)))
         (t t)))

;; UNUSED. only was used for Flatten, which now uses general scheme above
(defun mixima-check-first-arg-type ( call arg-spec)
 (cond ( (and (getf arg-spec ':arg1-list) ($mapatom (second call)))
         (format t "~a::normal Nonatomic expression expected at position 1 in ~a.~%"
                   (stripdollar (first (first call)))  ($sconcat call))
           nil)
       (t t)))


(defun mixima-check-position-spec ( call arg-spec  &aux pname epos-spec pos-spec pos-spec-n )
  (setf pos-spec-n (getf arg-spec :pos-spec-n  ))
  ( cond (  pos-spec-n
            (setf pos-spec (nth pos-spec-n call))
            (setq epos-spec (meval pos-spec))
            (cond ( (mixima-position-spec-p epos-spec)
                    t)
                  (t
                   (setf name (first (first call)))
                   (setq pname (stripdollar name))
                   (setq spos-spec ($sconcat epos-spec))
                   (setq scall ($sconcat call))
                   (format t "~a::psl: Position specification ~a in ~a~%    is not an integer or a list of integers.~%"
                           pname spos-spec scall )
                   nil)))
         (t t)))




|#  

