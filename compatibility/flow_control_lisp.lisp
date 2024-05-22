(defvar *mixima-flow-break-tag* nil)
;; Note we use gensym with *mixima-flow-break-tag* below. Probably not neccessary.

;; abort execution of body for this iteration
#|> Function Continue |#
(defmspec |$Continue| (x)
  (declare (ignore x))
  (throw *mixima-flow-break-tag* '(mixima-continue)))

;; abort execution of body and exit loop
#|> Function Break |#
(defmspec |$Break| (x)
  (declare (ignore x))
  (throw *mixima-flow-break-tag* '(mixima-break)))

;; abort execution of body and exit all enclosing For,While,etc. loops
;; Also exit enclosing Block
#|> Function Return |#
;; does not return the value of a local variable from Block.
;; but maxima return does.
;; So try to eval the return value first. We can also throw the mprog, which
;; kind of works, but it comes out unevaluated as well. I don't understand.
;; also can call mreturn. still does not evaluate a variable
(defmspec |$Return| (x)
  (cond
;;   ( (or mprogp mdop)
     ;;          (format t "In prog ~a~%" (mapcar 'meval (cdr x)))
;;     (mapply 'mreturn (cdr x) 'mreturn))
   (t
    (throw *mixima-flow-break-tag* (list 'mixima-return (mapcar 'meval (cdr x)))))))

(defun mixima-check-return (e)
  (and (listp e) (eq 'mixima-return (first e))))

(defun mixima-check-loop-break (e)
  (and (listp e) (or (eq 'mixima-break (first e))
                         (eq 'mixima-return (first e)))))

(defun mixima-get-return-value-or-nil (e)
  (if (and (listp e) (eq 'mixima-return (first e))) (meval (first (second e))) nil))

(defun mixima-eval-test-form (test)
  (eq (mfuncall '$is (meval test)) t))

#|> Function For |#
(mixima-set-attributes '|$For| '( |$HoldAll| ) )
(defmix |$For| ( start test incr (body :o) &aux bodyres )
  (meval start)
  (loop do
        (if (not (mixima-eval-test-form test)) (loop-finish))
        (let ((*mixima-flow-break-tag* (gensym)))
          (setf bodyres (catch *mixima-flow-break-tag* (meval body))))
        (if (mixima-check-loop-break bodyres) (loop-finish))
        (meval incr))
  (if (and *mixima-flow-break-tag* (mixima-check-return bodyres)) (throw *mixima-flow-break-tag* bodyres))
  (mixima-get-return-value-or-nil bodyres))


#|> Function While |#
(mixima-set-attributes '|$While| '( |$HoldAll|  ))
(defmix |$While| (test (body :o) &aux bodyres )
  (loop do
        (if (not (mixima-eval-test-form test)) (loop-finish))
        (let ((*mixima-flow-break-tag* (gensym)))
          (setf bodyres (catch *mixima-flow-break-tag* (meval body))))
        (if (mixima-check-loop-break bodyres) (loop-finish)))
  (if (and *mixima-flow-break-tag* (mixima-check-return bodyres)) (throw *mixima-flow-break-tag* bodyres))
  (mixima-get-return-value-or-nil bodyres))

;; This one will take some work. Mma and maxima are quite different in this respect.
;; Note Mma has $HoldRest, but we evaluate differently, so we have to do $HoldAll
;; Some general solution for this problem should be found
;; Do not meval condition immediately. We must do some substitutions first.
#|> Function If |#
(mixima-set-attributes '|$If| '( |$HoldAll| ))
(defmix |$If| (condition true-form (false-form :o) (undecided-form :o))
 (setq condition (mfuncall '$substitute '$equal "=" condition))
 (setq condition (meval condition))
 (setq condition (mfuncall '$substitute '$equal "=" condition))
 (setq condition (mfuncall '$is condition)) ; this won't get all behavior
 ( cond ( (eq t condition)
          (meval true-form))
        ( (null condition)
          (meval false-form))
        (t
         (if (null undecided-form) mix-call (meval undecided-form)))))

;; (defmspec |$For| ( x &aux bodyres)
;;   (cond  ( (mixima-check-args x '( :nargs-spec (3 4) ))
;;            (destructuring-bind
;;                (start test incr &optional (body nil)) (cdr x)
;;              (meval start)
;;              (loop do
;;                    (if (not (mixima-eval-test-form test)) (loop-finish))
;;                    (let ((*mixima-flow-break-tag* (gensym)))
;;                      (setf bodyres (catch *mixima-flow-break-tag* (meval body))))
;;                    (if (mixima-check-loop-break bodyres) (loop-finish))
;;                    (meval incr))
;;              (if (and *mixima-flow-break-tag* (mixima-check-return bodyres)) (throw *mixima-flow-break-tag* bodyres))
;;              (mixima-get-return-value-or-nil bodyres)))
;;            (t x)))

;; also works
;; (defmix |$While| (test (body :o))
;;   (let (bodyres)
;;     (loop do
;;           (if (not (mixima-eval-test-form test)) (loop-finish))
;;           (let ((*mixima-flow-break-tag* (gensym)))
;;             (setf bodyres (catch *mixima-flow-break-tag* (meval body))))
;;           (if (mixima-check-loop-break bodyres) (loop-finish)))
;;     (if (and *mixima-flow-break-tag* (mixima-check-return bodyres)) (throw *mixima-flow-break-tag* bodyres))
;;     (mixima-get-return-value-or-nil bodyres)))


;; this works fine, but trying defmix
;; (defmspec |$While| (x &aux bodyres)
;;   (cond  ( (mixima-check-args x '(  :nargs-spec (1 2) ))
;;            (destructuring-bind
;;                (test &optional (body nil)) (cdr x)
;;              (loop do
;;                    (if (not (mixima-eval-test-form test)) (loop-finish))
;;                    (let ((*mixima-flow-break-tag* (gensym)))
;;                      (setf bodyres (catch *mixima-flow-break-tag* (meval body))))
;;                      (if (mixima-check-loop-break bodyres) (loop-finish)))
;;              (if (and *mixima-flow-break-tag* (mixima-check-return bodyres)) (throw *mixima-flow-break-tag* bodyres))
;;              (mixima-get-return-value-or-nil bodyres)))
;;          (t x)))
