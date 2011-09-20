(in-package :cl-games-battleship)

(defmacro fof (&rest forms)
    (if forms
	(let ((form-result (gensym))
		(form (car forms)))
	  `(let ((,form-result ,form))
		(if ,form-result
		    ,form-result
		    (fof ,@(cdr forms)))))))

(defun square-root-of-squares-summ (&rest arguments)
  (sqrt (loop for argument in arguments summing
	    (* argument argument))))

(defmacro collect-the-lowest-level (form &rest levels)
  `(apply #'append
	  (loop for ,(car (car levels)) ,@(cdr (car levels))
	     collecting
	       ,(if (= 1 (length levels))
		    `(list ,form)
		    `(collect-the-lowest-level ,form ,@(cdr levels))))))

(defmacro sth-list (list sth)
  `(make-list (length ,list) :initial-element ,sth))