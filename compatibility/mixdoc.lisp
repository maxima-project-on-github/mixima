(defvar *mix-help-hash*  (make-hash-table :test #'equal  ))

;(load "./function_list.lisp")

;(defun dump-help-db ()
;  (maphash #'print-hash-entry *mix-help-hash*))

(defun get-hash-keys (hash)
  (let ((klist ()))
    (maphash (lambda (x y) (declare (ignore y)) (setf klist (cons x klist))) hash)
    klist))

(defun print-list-of-items (items)
    (format t "~{~<~%~1,80:;~A~> ~}"
            (sort items #'string-lessp))
    (format t "~%"))

;(defun print-help-items ()
;  (print-list-of-items (get-hash-keys *mix-help-hash*)))

;; Omit functions which reference nonexistent variable MIXIMA-FUNCTION-LIST. RHD 2024-05-16
#|
(defun print-help-items ()
  (print-list-of-items mixima-function-list))

(defun print-searched-help-items (str)
  (print-list-of-items (search-for-items mixima-function-list str)))
 |#

;(defun print-searched-help-items (str)
;  (print-list-of-items (search-for-items (get-hash-keys *mix-help-hash*) str)))

; return output list filtered by string str
(defun search-for-items (items str)
 (remove-if (complement #'(lambda (x) (search str x :test #'char-equal))) items))

(defun get-item (key)
  (gethash key *mix-help-hash*))

(defun set-item (key entry)
  (setf (gethash key *mix-help-hash*) entry))

(defun init-hash-item (key table)
  (setf (gethash key table)
        (list :name key :examples nil)))

(defun help-item-init (key)
  (init-hash-item key *mix-help-hash*))

(defun help-item-add-example (key example &aux entry)
  (setf entry (get-item key))
  (setf (getf entry  :examples) (cons example (getf entry :examples)))
  (set-item key entry))

(defun print-one-example (example &aux mma maxin maxout comments type)
  (declare (ignore type))
  (setf comments (getf example :comments))
;  (setf type (getf example :type))
;  (if type (format t "type: ~a~%" type))
  (if comments (format t "~a~%" comments))
  (setf mma (getf example :mma))
  (if (and mma (not (equalp mma ""))) (format t   "Mma   : ~a~%" mma))
  (setf maxin (getf example :maxin))
  (if (and maxin (not (equalp maxin ""))) (format t "maxima: ~a~%" maxin))
  (setf maxout (getf example :maxout))
  (if (and maxout (not (equalp maxout ""))) (format t "output: ~a~%" maxout))
  (format t "---------------------------~%"))

(defun print-examples (examples &key exclude include &aux type )
  (mapcar (lambda (x)
            (setf type (getf x :type))
            (cond ( (and include (not (equalp type include))))
                  ( (and exclude (equalp type exclude)))
                  ( t
                    (print-one-example x)))) examples)
  t)

(defun print-item-examples (item-name &key exclude include descr  &aux entry)
  (setf entry  (get-item item-name))
  (format t "~%~%---------------------------------~%")
  (format t "-- Function -- ~a~%" item-name)
  (format t "~a~%" descr)
  (format t     "---------------------------------~%~%")
  (print-examples (getf entry :examples) :exclude exclude :include include   ))


(defun count-item-types (item-name &key include exclude &aux entry examples type)
  (setf entry  (get-item item-name))
  (setf examples (getf entry :examples))
  (count-if  (lambda (e)
           (setq type (getf e :type))
           (cond ( (and include (not (equalp type include))) nil)
                 ( (and exclude (equalp type exclude)) nil)
                 ( t  t))) examples))


(defun print-item-bad-examples (item-name )
  (if ( >  (count-item-types item-name :exclude "ok") 0)
      (print-item-examples item-name :exclude  "ok" :descr "-- Unimplemented or broken Examples --")))

(defun print-item-good-examples (item-name )
  (if ( >  (count-item-types item-name :include "ok") 0)
      (print-item-examples item-name :include  "ok" :descr "-- Working Examples --")))


; this damn function is somewhere else ,  too
; Only switch the case if it is not mixed
(defun switch-case-maybe (str &aux upstr downstr)
  (setf upstr    (string-upcase str))
  (setf downstr  (string-downcase str))
  (cond ( (equal str upstr) downstr)
        ( (equal str downstr) upstr)
        ( t str )))

; was this causing a problem ?
(in-package :maxima)

;; Omit function mixFuncs since it calls PRINT-SEARCHED-HELP-ITEMS. RHD 2024-05-16
#|
#|> Function mixFuncs |#
; search for names of compatibility functions matching
; a string
(defmfun |$mixFuncs| ( &optional (search-str "")  )
  (if (symbolp search-str) (setf search-str (string (stripdollar search-str))))
  (print-searched-help-items search-str)
  t)
 |#

#|> Function mixDoc |#
; Example: mixDoc(Table)
(defmfun |$mixDoc| ( item &optional type )
  (if (not (stringp item)) (setq item (switch-case-maybe (subseq (format nil "~a" item) 1 ))))
;  (format t "~a~%" item)
  (cond ( (or (eq type '$GOOD) (eq nil type))
          (print-item-good-examples item))
        ( (eq type '$BAD)
          (print-item-bad-examples item)))
  t)
        
