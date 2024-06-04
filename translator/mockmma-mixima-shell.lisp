#|

  VERSION VERSIONNUMBER

  Based on code by Richard Fateman.

  Modified code Copyright (C) 2008, John Lapeyre  All rights reserved.

  This program is free software; you can redistribute it
  and/or modify it only under the terms of the GNU General
  Public License version 2 as published by the Free Software
  Foundation.

  Mathematica is a registered trademark of Wolfram Research,
  Inc. (2008)

  This code runs a shell from within the maxima shell that reads
  mma code, translates to maxima, evaluates, translates back to
  mma and prints.

|#


(if (find-package :mma) t   (defpackage :mma (:nicknames "MockMMA") (:use :common-lisp
					      #+allegro     :excl
					      )))
(in-package :mma)

;; Assume that set_file_search_paths has been called already to establish search path for Lisp files.

(maxima::$load "jmma.lisp")
(maxima::$load "uconsalt.lisp")
(maxima::$load "jmma.lisp")
(maxima::$load "builtinsyms.lisp")
(maxima::$load "mixima-mockmma-parser.lisp")
(maxima::$load "parser_patch.lisp")
(maxima::$load "mma-to-mixima.lisp")
(maxima::$load "stack1.lisp")
(maxima::$load "poly.lisp")
(maxima::$load "rat1.lisp")
(maxima::$load "simp1.lisp")
(maxima::$load "nmatrix.lisp")
(maxima::$load "eval.lisp")
(maxima::$load "pf.lisp")
(maxima::$load "disp1.lisp")
(maxima::$load "match.lisp")
(maxima::$load "maxima.lisp")
(maxima::$load "bf.lisp")

(defvar MIXIMA-MMA-COUNT 1)
(defvar mixima-trans t) ; whether to print translations when in mockmma shell with mixima
(defvar mixima-mma-verbose-flag nil) ; whether to print mma translation, etc.
(defvar mixima-reverse-trans t) ; whether to print the reverse translation in mockmma shell with mixima
(defvar mixima-print-ex nil ) ; whether to print an example from the input
(defvar mixima-print-ex-string "" ) ; string to print for example
(defvar mixima-entry-count 0)
(defvar mixima-mma-shell-debug-flag nil)

#|
(defun mixima-trans-command ( &aux minp max-input-string max-out-expr )
  (loop do
	(format t "In[~a]:= " MIXIMA-MMA-COUNT)
	(setf minp (mma::mma-parser))
					;   (format t "Got '~a'~%" minp)
	(cond ((equalp '|Exit|' minp) (loop-finish)))
	(setf max-input-string (mma::macsyma-source (mma::to-macsyma-expr minp)))
	(cond ((equalp "Exit" max-input-string) (loop-finish)))
	(format t " >> ~a~%" max-input-string)
	(setf max-out-expr (mfuncall '$eval_string max-input-string))
;	($print (format nil "Out[~a]:= " MIXIMA-MMA-COUNT) max-out-expr)
	(format t "Out[~a]:= " MIXIMA-MMA-COUNT)
	($max_to_mma max-out-expr)
	(setf MIXIMA-MMA-COUNT (1+ MIXIMA-MMA-COUNT)))
  t)
|#

(defun mixima  (&key mockmmaeval) ;; top level
  (let*
      ( (*package* (find-package :mma))
	(env (make-stack :size 50));environment for matching binding
	)
    (declare (special env *package*))
					;    (maxima::mfuncall 'maxima::$load "mockmma/mockmma.mac")
    (untrace))
;;    (if  mixima-mma-verbose-flag (trace maxima::meval) ))
  (if (= MIXIMA-MMA-COUNT 1)
      (format t 
	      "Mixima VERSION VERSIONNUMBER 
Distributed under the GNU General Public License."))
;  (clear-input ) ; does not seem to help
;  (if (listen) (format t "2 Char avail~%"))
  (mixima-read-eval-print-loop))


; This is entered via maxima fncn tomma()
(in-package :maxima)
(defmfun $entermma ( &rest args )
;  (format t "args to entermma ~a~%" args)
;  (format t "first args to entermma ~a~%" (first args))
  (setf mma::mixima-entry-count (first args))
  (setf mma::MIXIMA-MMA-COUNT (+ 1 $linenum))
  (format t "Entering mockmma shell. count:~a~%" mma::mixima-entry-count)
;  (cond ( (eq 0 (length args))
;          (setf mma::mixima-mma-verbose-flag nil))
;        ( t
;          (setf mma::mixima-mma-verbose-flag nil)))
  (mma::mixima))

(in-package :mma)

(defun mixima-mma-shell-debug (s)
;  (format t "mixima-shell: ~a~%" s)
  (if  mixima-mma-shell-debug-flag (format t " mixima-shell: ~a.~%" s)))

(defun mixima-read-eval-print-loop (&optional (stream t) &aux parsed-with-input parsed-mma-input
                                          mma-input-string max-input-string
                                          max-internal
                                          max-out-expr mma-out-expr(firsttime t)
                                          mma-parser-error
                                          mma-parser-error-flag
                                          mma-parser-error-val
                                          mma-parser-error-string
	                                      (mmaprefix "") (mmasuffix "")
                                          trailing-semicolon-flag hs )
  (when (find-package :maxima)
    (setf mmaprefix maxima::*prompt-prefix*)
    (setf mmasuffix maxima::*prompt-suffix*))
  (loop
;   (setf mixima-mma-verbose-flag t)
;   (setf mixima-mma-shell-debug-flag t)
   (catch 'mma::to-mix-repl 
     ;; Setting *DEBUGGER-HOOK* here makes it impossible to break out of this loop.
     ;; (setf *debugger-hook* #'mma::mix-mma-repl)
                                        ; there is something in the input stream the first time through
     (if (or (not firsttime) (> mixima-entry-count 0))
         (format t "~%~aIn[~s]:= ~a" mmaprefix MIXIMA-MMA-COUNT mmasuffix))
     (setf parsed-with-input  (mma::mma-parser stream))
     (setf firsttime nil)
;     (format t " parser returned:  <~a>~%" parsed-with-input)
     (cond ((eq (length parsed-with-input) 3)
             (setf mma-parser-error  (first parsed-with-input))
             (setf mma-parser-error-flag  (first mma-parser-error))
             (setf mma-parser-error-val  (second mma-parser-error))
             (setf mma-parser-error-string  (third mma-parser-error))
             (setf parsed-mma-input  (second parsed-with-input))
             (setf mma-input-string  (third parsed-with-input))
             (cond ( mma::mixima-mma-verbose-flag 
                     (format t "mma parser error code:  <~a>~%" mma-parser-error)
                     (format t "mma input string: <~a>~%" mma-input-string)
                     (format t "parsed mma: <~a>~%" parsed-mma-input)))
             (setf trailing-semicolon-flag (and (listp parsed-mma-input) (equal '|CompoundExpression| (first parsed-mma-input))
                                                       (equal '(NULL) (last parsed-mma-input))))
             (cond ( mma-parser-error-flag
                     (mixima-mma-shell-debug (format nil " translating string ~a" parsed-mma-input))
                     (setf max-internal (mma::to-macsyma-expr parsed-mma-input))
                     (if mma::mixima-mma-verbose-flag (format t "max internal : ~a~%" max-internal))
                     (setf max-input-string (mma::macsyma-source max-internal))
                     (setf mma-out-expr (let ((maxima::errset nil)) (let ((foo (maxima::errset (meval parsed-mma-input)))) (if foo (first foo) (list 'Hold parsed-mma-input)))))
                     (if (eq t mixima-trans)  (format t " (%i~a) ~a~%"  MIXIMA-MMA-COUNT max-input-string))
                                        ;                     (if  mma::mixima-mma-verbose-flag (trace maxima::meval) )
                     (mixima-mma-shell-debug (format nil " sending string to errcatch ~a" max-input-string))
                     (setf max-out-expr (maxima::mfuncall 'maxima::$mixerrcatch_eval_string max-input-string))
                     (mixima-mma-shell-debug (format nil " errcatch gave ~a" max-out-expr))
                     (cond ( (> (length max-out-expr) 1)
                             (mixima-mma-shell-debug " found length of return > 1")
                             (setf max-out-expr (second max-out-expr))
                             (mixima-mma-shell-debug (format nil " now maxout is ~a" max-out-expr))
;                     (setf max-out-expr (maxima::mfuncall 'maxima::$eval_string max-input-string))
                             (setf (symbol-value (intern (concatenate 'string "$%O" (format nil "~a" MIXIMA-MMA-COUNT)) "MAXIMA"))
                                   max-out-expr)
                             (setf (symbol-value (intern (concatenate 'string "$%I" (format nil "~a" MIXIMA-MMA-COUNT)) "MAXIMA"))
                                   (maxima::mfuncall 'maxima::$parse_string max-input-string))
                             (setf (symbol-value (intern (concatenate 'string "$%IS" (format nil "~a" MIXIMA-MMA-COUNT)) "MAXIMA"))
                                   mma-input-string)
                             (setf (symbol-value (intern (concatenate 'string "$%MIS" (format nil "~a" MIXIMA-MMA-COUNT)) "MAXIMA"))
                                   max-input-string)
                             (cond ( (and (eq t mixima-trans) (not trailing-semicolon-flag))
                                     (format t " (%o~a) "  MIXIMA-MMA-COUNT)
                                     (maxima::$print max-out-expr)))
                                        ;   (setf mma-out-expr(|MaxEval|  parsed-mma-input))
                             (setq mma-out-expr (mma::max2math max-out-expr))
                             (cond ( (or (equal "Exit" max-input-string) (equal "tomaxima" max-input-string) )
                                     (setf *debugger-hook* nil)
                                     (untrace)
                                     (return t))
                                   ( t
                                     (if mixima-print-ex (do-mixima-print-ex mma-input-string  max-input-string max-out-expr))
                                     (|SetQQ| (ulist '|Out| MIXIMA-MMA-COUNT) mma-out-expr)
                                     (cond((eq mma-out-expr'|Null|) nil)
                                          ;; don't print nil-valued answers
                                          ((eq t mixima-reverse-trans)
                                           (setf hs (list 
                                             '|Set|
                                             (ulist '|Out| MIXIMA-MMA-COUNT) mma-out-expr))
                                           (if mma::mixima-mma-verbose-flag (format t "~%Evaluated mma-out-expr: ~a" mma-out-expr))
                                           (if mma::mixima-mma-verbose-flag (format t "~%Evaluated mma-out line: ~a" hs))
                                        ;(dispwx MIXIMA-MMA-COUNT mma-out-expr) ; Call this for wxMaxima
                                        ;         (format t "format:   ~s" (BuildFormat hs))
                                        ;	 (format t "~%")
;;                                           (format t "~%Math2Max: ~a~%" (|Math2Max| parsed-mma-input))
                                           (cond ( (not trailing-semicolon-flag)
                                                   (disp (BuildFormat hs))
                                                   (format t "~%") )))))) ; Call this for command line Maxima
                             (setf MIXIMA-MMA-COUNT (1+ MIXIMA-MMA-COUNT))
                             (setf maxima::$linenum MIXIMA-MMA-COUNT)
                             (mixima-mma-shell-debug " maxima returned a value."))
                           (t (mixima-mma-shell-debug " maxima error, no value returned.")))
                     (mixima-mma-shell-debug " mma parser flagged no error."))
                   (t (cond ( (> mma-parser-error-val 1) ; don't print spurious NULLS
                             ;; (setf firsttime t) ; can't clear input stream, so flag to skip next null input
                              (clear-input t) ;; hmm, looks like I needed the argument t, maybe depends on lisp impl
                              (format t "mixima-shell: mockmma parser error ~a: ~a~%"
                                      mma-parser-error-val  mma-parser-error-string)))
                      (mixima-mma-shell-debug " mma parse error, finished loop.")))
             (mixima-mma-shell-debug " mma parser did return 3 items."))
           (t (mixima-mma-shell-debug " mma parser failed to return 3 items."))))
   (cond  ( (and (listen) (equal 'eof (peek-char nil nil nil 'eof))) ;; useless for interactive. doesn't work for piped
            (return t))
          (t 
;;           (format t "no eof~%") trying to detect eof on piped input. No luck
           t))))


     
     


; Print results to use in test and example
; currently don't know how to write InputForm Mma
(defun mma::do-mixima-print-ex ( mma-in  mix-in  mix-out )
  (format t "/* ex ok  ~a  */~%" mixima-print-ex-string)
  (format t "/* mma  ~a" mma-in )
;  (disp (BuildFormat mma-in))
  (format t " */~%~a;~%" mix-in)
  (maxima::$print mix-out (string ";"))
  (format t "/* endex */~%") )

(defun mix-eval-mma-string (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Mockmma string reading encountered an error :~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%")
  (throw 'mma::to-mix-eval-string t)
  )

(defun mix-mma-repl (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&mixima-shell encountered a lisp error :~%~% ~A" condition)
;;  (format t "~&Continuing.~%")
  (throw 'mma::to-mix-repl t)
  )

; read Mma from a string, translate and evaluate
#|> Function smmatomax |#
(maxima::defmfun maxima::$smmatomax (s &aux parsed-with-input parsed-mma-input
                                       mma-input-string max-internal max-input-string
                                       max-out-expr mma-parser-error
                                       mma-parser-error-flag
                                       mma-parser-error-val
                                       mma-parser-error-string)
                 (catch 'mma::to-mix-eval-string
                   (setf *debugger-hook* #'mma::mix-eval-mma-string)
                   ;; convert newlines to spaces so that more multiline input succeeds.
                   ;; add a space at the end so we don't get an error trying to read ahead.
                   (setf s (substitute #\Space #\Newline s))
                   (setf parsed-with-input (with-input-from-string (stream (concatenate 'string s " "))
                                                                   (mma::mma-parser stream))))
                                        ;                 (format t " parser returned:  <~a>~%" parsed-with-input)
                 (setf *debugger-hook* nil)
                 (cond ( (eq (length parsed-with-input) 3)
                         (setf mma-parser-error  (first parsed-with-input))
                         (setf mma-parser-error-flag  (first mma-parser-error))
                         (setf mma-parser-error-val  (second mma-parser-error))
                         (setf mma-parser-error-string  (third mma-parser-error))
                         (setf parsed-mma-input  (second parsed-with-input))
                         (setf mma-input-string  (third parsed-with-input))
                         (mma::init-translation)
                         (setf max-internal (mma::to-macsyma-expr parsed-mma-input))
                         (setf max-input-string (mma::macsyma-source max-internal))
;                         (setf max-input-string (mma::macsyma-source (mma::to-macsyma-expr parsed-mma-input)))
                         (cond ( mixima-mma-verbose-flag 
                                 (format t "mma parser error code:  <~a>~%" mma-parser-error)
                                 (format t "mma input string: <~a>~%" mma-input-string)
                                 (format t "parsed mma: <~a>~%" parsed-mma-input)
                                 (format t "max internal : ~a~%" max-internal)
                                 (format t "maxima input: \"~a\"~%" max-input-string)))
                         (cond ( mma-parser-error-flag
                                   (setf max-out-expr
                                         (maxima::mfuncall 'maxima::$mixerrcatch_eval_string max-input-string))
                                   (cond ( (> (length max-out-expr) 1)
                                           (setf max-out-expr (second max-out-expr)))
                                         (t (setf max-out-expr nil)))
                                   max-out-expr)
                               ( t
                                 (format t "smmatomax: mockmma parser error ~a: ~a~%~%" mma-parser-error-val
                                         mma-parser-error-string)
                                 nil)))
                       ( t nil)))
                 
#|> Function miximaTransFile |#
(maxima::defmfun maxima::|$miximaTransFile| (f)
		 (mma::read-trans-print-file f)
		 t)

#|> Function miximaEvalFile |#
(maxima::defmfun maxima::|$miximaEvalFile| (f)
  (maxima::mfuncall 'maxima::$eval_string (mma::stfile f)))

#|> Function MmaVerbose |#
(maxima::defmfun maxima::|$MmaVerbose| ( &rest x)
 (cond ( (and (listp x) (eq 'maxima::|$False| (first x)))
         (format t "Unsetting mma verbose~%")
	 (setf mma::mixima-mma-verbose-flag nil))
       (t
        (format t "Setting maxima verbose~%")
	(setf mma::mixima-mma-verbose-flag t))))

#|> Function MmaShowTrans |#
(maxima::defmfun maxima::|$MmaShowTrans| ( &rest x)
 (cond ( (and (listp x) (eq 'maxima::|$False| (first x)))
         (format t "Unsetting maxima translation~%")
	 (setf mma::mixima-trans nil))
       (t
        (format t "Setting maxima translation~%")
	(setf mma::mixima-trans t))))

#|> Function MmaShowOut |#
(maxima::defmfun maxima::|$MmaShowOut| ( &rest x)
; (format t "got ~a~%" x)		 
; (format t "got lis ~a~%" (first x))		 
 (cond ( (and (listp x) (eq 'maxima::|$False| (first x)))
         (format t "Unsetting reverse translation~%")
	 (setf mma::mixima-reverse-trans nil))
       (t
        (format t "Setting reverse translation~%")
	(setf mma::mixima-reverse-trans t))))

#|> Function MmaPrintEx |#
(maxima::defmfun maxima::|$MmaPrintEx| ( &rest x )
;                 (format t "Prinex arg '~a'~%" x)
                 (setf x (meval (first x)))
                 (cond ( (eq 'maxima::|$False| x)
                         (setf mma::mixima-print-ex nil))
                       ( t
                         (if (null x) (setf mixima-print-ex-string "")
                           (setf mixima-print-ex-string (format nil "~a" x)))
                         (format t "Enabling printing of examples~%")
                         (setf maxima::$display2d nil)
                         (setf mma::mixima-print-ex t))))

(in-package :maxima)
(import 'mma::mixima)
