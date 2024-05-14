#|> Function Simplify |#
;; Loop through simplifications till result no longer changes
;; These are not chosen at random, but to attempt to reproduce Mma Simplify
;; This is crude. Could look at leafcount like they do.
(defmfun |$Simplify| (e &rest args &aux r1 r2 (count 0) (countlim 5)) 
  ;; ARGS might plausibly supply flags or something, although it's not used at present
  (declare (ignore args))
  (setq r1 e)
  (loop do 
;	(format t "Count ~a~%" count)
	(setq count (1+ count))
	(setq r2 (mfuncall '$ratsimp r1))
	(setq r2 (mfuncall '$trigsimp r2))
	(setq r2 (mfuncall '$trigreduce r2))
	(setq r2 (mfuncall '$trigrat r2))
	(if (equalp r2 r1) (loop-finish)
	  (setq r1 r2))
	(if (> count countlim) (loop-finish)))
  r2)

