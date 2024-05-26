#|

  All block-like constructs from Mma are replaced by maxima block().
  For example, I believe variables declared in Block are dynamically- and in Module lexically-
  scoped. So this distinction is lost.

  There are two versions here: mixima-blocklike-to-block and old-mixima-blocklike-to-block.
  The old one replaces  => with : in the variable list to satisfy error checking in mprogn.
  But instead, we modified mprogn to allow => and now use the more simple mixima-blocklike-to-block

  In old-mixima-blocklike-to-block things
  are of getting out of control. This picks out the assigment statements '=>'  and replaces with ':' in
  the first statement in the block, because only real assignments are allowed.

|#


;; mixprog already has a catch in it, but that doesn't catch Return()
;; If the value thrown by Return is local to the mixprog (ie mprog) then
;; we don't see that value here.
;; maxima return gets it correctly
(defun mixima-blocklike-to-block (x &aux res )
  (setq x (cdr x))
  (let ((*mixima-flow-break-tag* (gensym)))
    (setf res (catch *mixima-flow-break-tag* (meval (cons '(mixprog simp) x)))))
  (if (mixima-check-return res) (mixima-get-return-value-or-nil res) res))

;; should move arg checking to function above 
(defmspec |$With| (x)
  (cond  ( (mixima-check-args x '(  :nargs-spec 2 ))
           (mixima-blocklike-to-block x))
         (t x)))

(defmspec |$Module| (x)
  (cond  ( (mixima-check-args x '(  :nargs-spec 2 ))
           (mixima-blocklike-to-block x))
         (t x)))

(mixima-set-attributes '|$Block| '( (|$HoldAll| t) ))
(defmix |$Block| (argl body)
  (declare (ignore argl body))
  (mixima-blocklike-to-block mix-call))

;;(defmspec |$Block| (x)
;;  (cond  ( (mixima-check-args x '(  :nargs-spec 2 ))
;;           (mixima-blocklike-to-block x))
;;         (t x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Old version. all rtests pass with this at the time it was disabled by prepending old- . version control.  bzr revno 84.
;; We modify progn and use the simpler code above.
;;
(defun old-mixima-blocklike-to-block (x &aux plen stmt1 nstmt1 )
  (setq x (cdr x))
  (setq plen (length x))
  (cond ( (> plen 0)
          (setq stmt1 (first x))
;          (format t "  stmt1 ~a ~%" stmt1 )
          (cond ( ($listp stmt1)
                  (setq nstmt1 (mapcar #'(lambda (y)
                                   ( cond ( (listp y)
                                            (substitute-if '(msetq) #'(lambda (x) (equalp x '($=>))) y))
                                          ( t y )))  stmt1 ))
;                  (format t "stmt1 ~a, nstmt ~a~%" stmt1 nstmt1 )
                  (meval (cons '(mprog simp) (cons nstmt1 (rest x)))))))
        (t
         (meval (cons '(mprog simp) x)))))

