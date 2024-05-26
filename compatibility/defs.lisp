(defvar *mixima-attributes* (make-hash-table))

;; I give up. doing this imperatively. at least it works.
;; args is the parameter specification to defmix
;; There are three kinds of parameters, required, optional, and options
;; Group them into a plist of lists keyed with :req :optional :rule
;;
;;  changed this!    (:o c) is now (c :o)
;; these examples are out of date
;; (mixima-group-args  '( a b c))
;;   (:REQ ((:N A) (:N B) (:N C)) :OPTIONAL NIL :RULE NIL)
;; (mixima-group-args  '(a b (:o c) (:o d)))
;;   (:REQ ((:N A) (:N B)) :OPTIONAL ((:O C) (:O D)) :RULE NIL)
;; (mixima-group-args  '(a b (:o c) (:o d) (:r r1) (:r r2)))
;;   (:REQ ((:N A) (:N B)) :OPTIONAL ((:O C) (:O D)) :RULE
;;       ((:R R1) (:R R2)))

#|
 take arg specification to defmix and analyze it a bit. If an element is
 symbol -- required arg
 list  -- first element must be symbol that will be bound for required arg
      remaining args are keys :keyn
      :o optional arg
      :oo collect remaining args in list of optional args
      :r   rule
     The arg name (first elmt) and :o,:oo,:r are stripped from list and remaining
     keys are interpreted as names of tests that the arg must pass at call time.
     They are collected along with the argument position (argc '(remaining keys))
     and these in turn are collected in a list that is put in the plist under
     :arg-tests. This list of argument-position and arg-test pairs is passed at
     call time to an argument checking routine.
|#

(defun mixima-group-args (args &aux req-args optional-args
                               optional-rest aux-args rules aux-flag  (argc 0) tests test-list )
  (loop for e in args do
        (incf argc)
        (cond ( (listp e )
                (setf tests (remove-if #'(lambda (x) (member x '(:o :oo :r))) (rest e)))
                (if tests (setf test-list (cons (cons argc tests) test-list))))
              (t t))
        (cond ( (eq e '&aux )
                (decf argc)
                (setf aux-flag t))
              ( aux-flag
                (setq aux-args (cons e aux-args)))
              ( (atom e)
                (setq req-args (cons (list e) req-args))) ; canonicalize
              ( (member ':o e)
                (setq optional-args (cons e optional-args)))
              ( (member ':oo e)
                (setq optional-rest (cons e optional-rest)))
              ( (member ':r e)
                (setq rules (cons e rules)))
              ( t (setq req-args (cons e req-args))))) ; default is required argument
;;  (format t "testlist ~a~%" test-list)
  (list :req (reverse req-args) :optional (reverse optional-args) :optional-rest (reverse optional-rest)
        :rule (reverse rules) :aux (reverse aux-args) :arg-tests (reverse test-list) ))

;; Return the minimum and maximum number of allowed non-option (ie rule) args.
;; This is computed from the lengths of the grouped args.
;; This is used in the first argument test that Mma does at runtime
;; Eg, return (3 5) if 3 to 5 args are expected
;; If optional args are collected with &rest, then return nil which causes
;; narg checking routine to do nothing.
(defun mixima-min-max-nargs (grouped-args)
  (cond ( (getf grouped-args :optional-rest )
          nil)
        (t
         (let ( (n   (length (getf grouped-args :req))))
           (list n (+ n (length (getf grouped-args :optional))))))))

;; Create a lisp defun parameter-specification from a defmix parameter
;; specification.  A defmix expands to  a defmspec within which the entire call form is
;; passed to mixima-check-args along with information taken from the defmix parameter
;; specification. If t is reuturned, then the form generated here is used for
;; a destructuring-bind around the main body.
;; Treating rules properly is not done yet here.
;; This should be made more efficient and pretty
(defun mixima-mk-param-list (grouped-args &aux n o oo r a)
  (setq n (mapcar #'(lambda (e) (first e )) (getf grouped-args :req)))
  (setq o (mapcar #'(lambda (e) (first e)) (getf grouped-args :optional)))
  (setq oo (mapcar #'(lambda (e) (first e)) (getf grouped-args :optional-rest)))
  (setq r (mapcar #'(lambda (e) (first e )) (getf grouped-args :rule)))
  (setq a (getf grouped-args :aux))
  (if (not (null o)) (setq n (append n (cons '&optional o))))
  (if (not (null oo)) (setq n (append n (cons '&rest oo))))
  (if (not (null r)) (setq n (append n '(&rest rules))))
  (if (not (null a)) (setq n (append n (cons '&aux a))))
  n)


;; Define a maxima function via defmspec, but add some code to control evaluation
;; check number of args, etc., in a way similar to Mma.
;; For instance, usually, instead of an error, Mma prints a message and returns
;; the unevaluated form (but with args evaluated according to specification).
;; If desired, we could have a globabl flag to call merror instead.
;; Currently try to evaluate an arg either zero or once according to spec; again,
;; we might experiment with a flag that causes eval loop to a fixed point as in Mma.
;; Ex:   (defmix |$SomeFunc| ( a b (:o c) (:o d) )  body-form1 body-form2 ... )
;; where (:o c) means c is optional.
;; Arg spec (:oo e) collects rest of args in list e (via &rest)
;; Two local lexical bindings are created, mix-call and mix-call-args
;; which are available in the funciton body if needed. But the destructuring of mix-call-args
;; is done automatically by the macro. Seems perhaps a bit convoluted. But the
;; Mma-style arg checking is done first and the input form is returned on failure as Mma does.
;; Currently only number-of-arg checking is done, although mixima-check-args already has
;; some other stuff.
;; Eg, need to add :pos-spec-n , as used in MapAt
;; Note: defmfun's can be called directly (|$Sfun| .... ) while
;;  defmspec's must be called via (mfuncall |$Sfun| .... )
;;  I don't understand the difference between these two. But replacing defmfun with defmix
;;  causes at least this problem.
(defmacro defmix (name args &rest body)
  (let (nargs-spec grouped-args param-list arg-tests)
  (setf grouped-args (mixima-group-args args))
  (setf nargs-spec (mixima-min-max-nargs grouped-args))
  (setf param-list (mixima-mk-param-list grouped-args))
  (setf arg-tests  (getf grouped-args :arg-tests ))
  `(defmspec ,name (mix-call &aux mix-call-args)
     ,(cond ( (mixima-get-attribute name '|$HoldAll|)
             t)
           ( (mixima-get-attribute name '|$HoldFirst|)
             '(setf mix-call (cons (first mix-call) (cons (second mix-call)  (mapcar #'meval (rest (rest mix-call)))))))
           ( (mixima-get-attribute name '|$HoldRest|)
             '(setf mix-call (cons (first mix-call) (cons (meval (second mix-call))  (rest (rest mix-call))))))
           (t
            '(setf mix-call (cons (first mix-call) (mapcar #'meval (rest mix-call))))))
     (cond ( (mixima-check-args mix-call  '(:nargs-spec ,nargs-spec :arg-tests ,arg-tests ))
             (setf mix-call-args (cdr mix-call))
             (destructuring-bind ,param-list mix-call-args
               ,@body))
           (t mix-call)))))

#|
 Exapmple: Flatten. evaluate all args before doing anything
    e is required
    nmax is optional
    e must fail $mapatom test
 (defmix |$Flatten| ( (e :natom) (nmax :o) )

 Not sure how to rtest for failing :natom test

|#

#| we can throw this out.
(defmacro defmix (name args &rest body nargs-spec grouped-args param-list callargs)
  (setf grouped-args (mixima-group-args args))
  (setf nargs-spec (mixima-min-max-nargs grouped-args))
  (setq param-list (mixima-mk-param-list grouped-args))
  `(defmspec ,name (mix-call &aux mix-call-args attrs)
     (cond ( (mixima-get-attribute ',name '|$HoldAll|)
             t)
           ( (mixima-get-attribute ',name '|$HoldFirst|)
             (setf mix-call (cons (first mix-call) (cons (second mix-call)  (mapcar #'meval (rest (rest mix-call)))))))
           ( (mixima-get-attribute ',name '|$HoldRest|)
             (setf mix-call (cons (first mix-call) (cons (meval (second mix-call))  (rest (rest mix-call))))))
           (t
            (setf mix-call (cons (first mix-call) (mapcar #'meval (rest mix-call))))))
     (cond ( (mixima-check-args mix-call  '(:nargs-spec ,nargs-spec))
             (setf mix-call-args (cdr mix-call))
             (destructuring-bind ,param-list mix-call-args
               ,@body))
           (t mix-call))))
|#

;; Either of the following are allowed.
;; (mixima-set-attributes '|$For| '( (|$HoldAll| t) ))
;; (mixima-set-attributes '|$For| '( |$HoldAll| ) )
(defun mixima-set-attributes (name attr-list &aux attrs)
  (setf attrs (setf (gethash (stripdollar name) *mixima-attributes*) (make-hash-table)))
  (setf attr-list (mapcar #'(lambda (x) (if (listp x) x (list x t))) attr-list))
  (mma::set-hash-elements attrs attr-list))

(defun mixima-get-attribute (name attr &aux attrh)
;;  (format t "mixima-get-attr ~a ~a~%" name attr)
;;  (format t " hash  ~a~%" (gethash (stripdollar name) *mixima-attributes*))
  (setf attrh (gethash (stripdollar name) *mixima-attributes*))
  (if attrh  (gethash attr attrh) nil))

#|> Function Attributes |#
;; return mlist of attributes of function f
(defmix |$Attributes| (f &aux attrh)
  (setf attrh (gethash (stripdollar f) *mixima-attributes*))
  ( if attrh (cons '(mlist) (mma::get-hash-keys attrh))
    '( (mlist ))))

;;;;;;;; Some examples

;; This test works ok
;;  (mixima-set-attributes '|$Tfunc| '( (|$HoldRest| t) ))
;; (defmix |$Tfunc| (a b c d)  (list '(mlist) a b c d))

;;(mixima-set-attributes '|$Tfunc| '( |$HoldAll|  ))
;; (defmix |$Tfunc| ( (:n a  :list )  (:o b) )
;;  (list '(mlist) a  ))

;; An example (that ignores its optional arg)
;; (defmix |$Map| (func form (:o spec))
;;  (mapply '$map (list func form)))

