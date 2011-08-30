(in-package :cl-games-battleship)

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

(defmacro defun-sphere-and-aura (dimensions)
  (let ((aura (intern (concatenate 'string
				   "AURA-"
				   (princ-to-string dimensions) "D")))
	(sphere (intern (concatenate 'string
				     "SPHERE-"
				     (princ-to-string dimensions) "D")))
	(dimensions (progn
		      (decf dimensions)
		      (loop for dimension upto dimensions
			 collecting (gensym)))))
    `(progn
       (defun ,sphere (position radius &key border content)
	 (remove-if #'null
		    (collect-the-lowest-level
		     (if (funcall (cond
				    (content '>)
				    (border '=)
				    (t '>=))
				  radius
				  (floor
				   (square-root-of-squares-summ
				    ,@(loop for dimension in dimensions
					 for n by 1
					 collecting
					 `(- (nth ,n position) ,dimension)))))
			 (list ,@dimensions))
		     ,@(loop
			  for dimension in dimensions
			  for n by 1
			  collecting
			    (list dimension
				  'from `(- (nth ,n position) radius)
				  'to `(+ (nth ,n position) radius))))))
       (defun ,aura (cells radius &key border)
	 (let ((answer
		(remove-duplicates
		 (apply #'append
			(loop for cell in cells collecting
			     (,sphere cell radius :border (if border
							      border))))))
	       (deletion (if border
			     (apply #'append
				    (loop for cell in cells
				       collecting
					 (,sphere cell radius
						  :content t))))))
	    (if border
		(remove-if #'(lambda (cell)
			       (find cell deletion
				     :test #'equal))
			   answer)
		answer))))))
