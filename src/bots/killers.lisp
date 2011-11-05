(in-package :cl-games-battleship)

(defun search-for-ship (just-shooted killed-ships)
  (if just-shooted
      (remove-duplicates
       (append (search-for-ship (find-if #'(lambda (cell)
					     (find cell (sphere just-shooted 1)
						   :test #'equal))
					 (remove just-shooted killed-ships))
				(remove just-shooted killed-ships))
	       (list just-shooted)
	       (search-for-ship (find-if #'(lambda (cell)
					     (find cell (sphere just-shooted 1)
						   :test #'equal :from-end t))
					 (remove just-shooted killed-ships))
				(remove just-shooted killed-ships)))
       :test #'equal)))

(defun random-killer (game-space-config ships-config)
  (declare (ignore ships-config))
  (let ((gsconfig game-space-config)
	killed-ships
	killed
	to-kill)
    #'(lambda (&optional result)
	(cond ((eql result :killed)
	       (push to-kill killed-ships)
	       (setf killed
		     (remove-duplicates
		      (append killed 
			      (aura (search-for-ship
				     to-kill killed-ships) 1)))))
	      ((eql result :shooted)
	       (push to-kill killed-ships)
	       (setf killed 
		     (remove-duplicates
		      (append killed (perforated-sphere to-kill))))))
	(loop
	   (setf to-kill (loop for coord in gsconfig collecting
			      (+ 1 (random coord))))
	   (unless (find to-kill killed :test #'equal)
	     (push to-kill killed)
	     (return to-kill))))))

(defun constant-killer (game-space-config ships-config)
  (declare (ignore ships-config))
  (let ((killing-sequence (cube (sth-list game-space-config 1)
				game-space-config))
	killed-ships
	killed
	to-kill)
    #'(lambda (&optional result)
	(cond ((eql result :killed)
	       (push to-kill killed-ships)
	       (setf killed
		     (remove-duplicates
		      (append killed 
			      (aura (search-for-ship
				     to-kill killed-ships) 1)))))
	      ((eql result :shooted)
	       (push to-kill killed-ships)
	       (setf killed 
		     (remove-duplicates
		      (append killed (perforated-sphere to-kill))))))
	(loop
	   (setf to-kill (pop killing-sequence))
	   (unless (find to-kill killed :test #'equal)
	     (push to-kill killed)
	     (return to-kill))))))

(defmacro random-pop (place)
  (let ((popped (gensym)))
    `(if (consp (cdr ,place))
	 (let ((,popped (nth (random (length ,place)) ,place)))
	   (setf ,place (remove ,popped ,place :test #'equal :count 1))
	   ,popped)
	 (pop ,place))))

(defmacro incn (n list &optional (increment 1))
  (let ((incfed-list (gensym)))
    `(let ((,incfed-list (copy-list ,list)))
       (incf (nth ,n ,incfed-list) ,increment)
       ,incfed-list)))

(defun random-grid (cube-config grid-step starting-cell)
    (if (< 1 grid-step)
	(let ((grid-step (loop for x = 2 then (* x 2) doing
			      (when (> x grid-step)
				(return (/ x 2))))))
	  (labels ((grid-constructor (cub-con gr-st start)
		     (if (every #'<= start cube-config)
			 (cons (if (every #'plusp start) start)
			       (apply #'append
				      (loop for n by 1
					 repeat (length start)
					 collecting
					   (grid-constructor
					    cub-con
					    gr-st
					    (incn n start
						  gr-st))))))))
	    (sort
	     (remove-duplicates
	      (remove-if
	       #'null 
	       (apply #'append
		      (loop for n upto (first (sort cube-config #'<)) collecting
			   (grid-constructor cube-config grid-step
					     (mapcar (lambda (x) (+ n x))
						     starting-cell)))))
	      :test #'equal)
	      (lambda (x y) (loop for n upto (1- (length x)) doing
				 (unless (= (nth n x) (nth n y))
				   (return (> (nth n x) (nth n y)))))))))
	(cube (sth-list cube-config 1)
	      cube-config)))

(defun strategic-random-killer (game-space-config ships-config)
 (let (killing-sequence
       (ships-config (sort (loop for x = 1 then (* x 2) collecting
			      x
			      while (< x (first (sort ships-config #'>))))
			   #'>))
       killed-ships
       (starting-cell (loop for x in game-space-config collecting
			   (- (random x))))
       killed
       to-kill)
    #'(lambda (&optional result)
	(cond ((eql result :killed)
	       (push to-kill killed-ships)
	       (setf killed
		     (remove-duplicates
		      (append killed 
			      (aura (search-for-ship
				     to-kill killed-ships) 1)))))
	      ((eql result :shooted)
	       (push to-kill killed-ships)
	       (setf killed 
		     (remove-duplicates
		      (append killed (perforated-sphere to-kill))))))
	(loop
	   (if (null killing-sequence)
	       (setf killing-sequence
		     (funcall
		      (if (= 0 (random 2))
			  #'identity
			  #'nreverse)
			  (random-grid game-space-config
				       (pop ships-config)
				       starting-cell)))
	       (progn
		 (setf to-kill (pop killing-sequence))
		 (unless (find to-kill killed :test #'equal)
		   (push to-kill killed)
		   (return to-kill))))))))

;; Intelligent killer

