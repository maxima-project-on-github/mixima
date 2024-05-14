;; done : Increment::argx: Increment called with 0 arguments; 1 argument is expected.
;; done : Increment::argx: Increment called with 2 arguments; 1 argument is expected.
;; done : Increment can operate on a list


#|
 In this implementation, when things are evaluated is not always the same, as in Mma particularly on failure.
 Some of the actual behavior seems not to be by design, but rather chance.
 In[1]:= a=1;b=100;Increment[a,b]
  Increment::argx: Increment called with 2 arguments; 1 argument is expected.
 Out[1]= Increment[a, 100]
|#

(defun mixima-incr-decr (x incr pre &aux var retval name prname form args)
;  (format t "call ~a~%" x)
  (setq name (first x)) ; a list
  (setq prname (stripdollar(first name)))
  (setq args (cdr x))
  (cond ( (eq (length args) 1)
          (setq var (first args))
;          (format t "args ~a~%" args)
          (cond  ( ($listp var)
                   (cons '(mlist) (map 'list (lambda (x) (mixima-incr-decr (list name x) incr pre)) (rest var))))
;;                 disabled this line, which allows more unusual stuff to succeed than Mma does,
;;                like unbound z --> z + 1. But also m:[1,2] and AddTo(m[1]) works. We could start checking for
;;                    more things to allow and disallow...
;;                 ( (and (symbolp var) (boundp var) (not (null var) ))
                 ( t
                   (if pre (setq retval (meval var)))
                   (setq form (list '($=> simp) var (list '(mplus simp) var incr)))
                   (cond ( pre
                           (meval form)
                           retval)
                         ( t
                           (meval form))))
                 (t (format t (format nil "~a::rvalue:~%  ~a is not a variable with a value,  so its value cannot be changed.~%"
                                      prname (mfuncall '$string var)))
                    x)))
        (t (format t (format nil "~a::argx: ~a called with ~r argument~:p; 1 argument is expected.~%"
                             prname prname (length args)  ))
           x)))

  
#|> Function Increment |#        
(defmspec |$Increment| (x)
  (mixima-incr-decr x  1 t))

#|> Function PreIncrement |# 
(defmspec |$PreIncrement| (x)
  (mixima-incr-decr x  1 nil))

#|> Function Decrement |# 
(defmspec |$Decrement| (x)
  (mixima-incr-decr x  -1 t))

#|> Function PreDecrement |# 
(defmspec |$PreDecrement| (x)
  (mixima-incr-decr x  -1 nil))


(defun mixima-op-to (x oper &aux var incr name prname args form)
;  (format t "call ~a, oper ~a~%" x oper)
  (setq name (first x)) ; a list
  (setq prname (stripdollar(first name)))
  (setq args (cdr x))
  (cond ( (eq (length args) 2)
          (setq var (first args))
;          (format t "args ~a, var ~a~%" args var)
          (setq incr (meval (second args)))
          (cond ( (eq oper 'mdivide)
                  (setq incr (list '(mexpt) incr -1))
                  (setq oper 'mtimes))
                ( (eq oper 'mminus)
                  (setq incr (list '(mtimes) -1 incr ))
                  (setq oper 'mplus)))
;          (format t " incr ~a~%" incr)
          (cond  ( ($listp var)
                   (cons '(mlist) (map 'list (lambda (y) (mixima-op-to (list name y incr) oper)) (rest var))))
;;                 ( (and (symbolp var) (boundp var) (not (null var) ))
                 ( t
                   (setq form (list '($=> simp) var (list (list oper) var incr)))
;                   (format t " form ~a~%" form)
                   (meval form))
                 (t (format t (format nil "~a::rvalue:~%  ~a is not a variable with a value,  so its value cannot be changed.~%"
                                      prname (mfuncall '$string var)))
                    x)))
        (t (format t (format nil "~a::argx: ~a called with ~r argument~:p; 2 arguments are expected.~%"
                             prname prname (length args)  ))
           x)))


#|> Function AddTo |# 
(defmspec |$AddTo| (x)
  (mixima-op-to x 'mplus ))

#|> Function SubtractFrom |# 
(defmspec |$SubtractFrom| (x)
  (mixima-op-to x 'mminus ))

#|> Function TimesBy |# 
(defmspec |$TimesBy| (x)
  (mixima-op-to x 'mtimes ))

#|> Function DivideBy |# 
(defmspec |$DivideBy| (x)
  (mixima-op-to x 'mdivide ))
