#|

  Mma pure functions. This is highly suboptimal. But it seems to be working.
  Most of the complexity and inefficiency comes from splicing SlotSequence.
  Not sure how to handle this.

  We can't just build the body of the lambda function using any lisp that computes
  the correct result. This is because the evaluator gets inside and errors occur when
  it fails to find maxima expressions. Same with printing the lambda function, this
  can cause crashes. Eg $mix_nth is used because of this.

  Implemented: 
   Function[{x,y} , x+y]
   Function[x , x^2]  same as    Function[{x} , x^2]

   #, #1 which are Slot(1)
   #2,  Slot(2)
   ##n  , SlotSequence(n)
   ##1 same as ##

   Here, we don't use # or ##, only Slot(n) and SlotSequence(n)

  #0 & "whole function" is not implemented

  Named args, ie Function[{x,y} , x+y] are implemented.
    How to distinguish one
    from the other? Looks like normally, Function with named args has two two
    args (arglist and body), while with Slot args there is only one arg to Function,
    the body. You can actually do hybrid, undocumented things in Mma , which we don't
    try to do here.

  Note the translator fails with  Function[x,x^2] [a],
  Translator should detect this and add mqapply.

  Still need to use gensym to protect name collision with mixima_lambda_args.

  Also, we use destructive replacement which could cause bugs.

|#

(defvar $mixima_lambda_args nil)

#|> Function Function |#
(defmspec |$Function| ( x &aux callargs arglist )
  (setf callargs (rest x))
  (cond ( (eq 2 (length callargs))
          (setf arglist (first callargs))
          (if (not ($listp arglist)) (setf arglist (list '(mlist simp) arglist)))
          (list  '(lambda simp) arglist (second callargs)))
        (t 
         (cons '(lambda simp) (cons '((mlist) ((mlist) $mixima_lambda_args))
                                    (repl-slot callargs))))))

;; this is only to keep maxima from segfaulting when it tries to
;; print (nth n e)
(defmspec $mix_nth (x)
  (nth (second x) (meval (third x))))

;; hmmm, should be passing args here, not using $mixima_lambda_args ?
;; Partition expression e into lists separated by SlotSequence(n). At the same time, make
;; lists of args for these SlotSequence(n). Collect all these in clists and then
;; append them. This effects the splicing.
(defmspec $mix_splice (x &aux e (ppos -1) clists )
  (setf e (third x))
  (dolist (pos (rest (second x)))
    (setf clists (cons (subseq e (1+ ppos) pos) clists))
    (setf clists (cons (subseq $mixima_lambda_args (second (nth pos e)) (length $mixima_lambda_args)) clists))
    (setf ppos pos))
  (setf clists (cons (subseq e (1+ ppos) (length e)) clists))
  (apply #'append (reverse clists)))


(defun repl-slot (e)
  (cond ( (atom e) e)
        ( (and (listp (first e)) (eq (caar e) '|$Slot|))
          `( ($mix_nth) ,(second e) $mixima_lambda_args))
        ( t
          (mapcar #'repl-slot (repl-slot-seq e)))))

;; e is a list. replace all occurences of 'calls' to SlotSequence with SlotHold, and
;; mark each position. Return code to call mix_splice that will splice in arguments at 'runtime'
;; at each marked position. If  af : Function(...) , then repl-slot-seq is only call once. But
;; if Function() is used in place of lambda, then this is called each time Function is called.
;; ... inefficient
(defun repl-slot-seq (e &aux pos posl)
  (loop do 
        (setf pos (position '|$SlotSequence| e :key #'(lambda (x) (and (listp x) (listp (first x)) (caar x) ))))
        (cond ( pos
                (setf (elt e pos) `( ( |$SlotHold| ) ,(second (nth pos e))))
                (setf posl (cons pos posl)))
              (t (loop-finish))))
  (cond ( posl
          (setf posl (cons '(mlist) (reverse posl)))
          `( ($mix_splice) ,posl ,e))
        (t e)))


#| ---------------------

The code here almost works. Only SlotSequence is
not spliced in properly. Too bad, its quite simple.
 
(defvar *mix-fun-par* nil)

(defmspec |$Function| ( x &aux callargs )
  (setf callargs (rest x))
  (let ( *mix-fun-par* )
    (cons '(lambda simp) (cons '((mlist) ((mlist) *mix-fun-par*)) callargs))))

(defun |$Slot| (n)
  (nth n *mix-fun-par*))

(defun |$SlotSequence| (n)
  *mix-fun-par*)

--------------------- |# 

