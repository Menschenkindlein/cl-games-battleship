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

;; Intelligent killer

