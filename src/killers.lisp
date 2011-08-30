(in-package :cl-games-battleship)

(defun constant-killer (&key game-space-config ships-config)
  (declare (ignore ships-config))
  (let ((gsconfig game-space-config)
	to-kill)
    #'(lambda (&optional result)
	(if (null result)
	      (setf to-kill '(1 1))
	      (if (= (first gsconfig)
		     (first to-kill))
		    (setf to-kill (list 1 (+ 1 (second to-kill))))
		    (setf to-kill (list (+ 1 (first to-kill))
					(second to-kill)))))
	(princ to-kill))))

(defun random-killer (&key game-space-config ships-config)
  (declare (ignore ships-config))
  (let ((gsconfig game-space-config)
	killed
	to-kill)
    #'(lambda (&optional result)
	(declare (ignore result))
	(loop
	   (setf to-kill (list (+ 1 (random (first gsconfig)))
			       (+ 1 (random (second gsconfig)))))
	   (if (not (find to-kill killed :test #'equal))
	       (progn (push to-kill killed)
		      (return to-kill)))))))

(defun search-for-ship (just-shooted killed-ships)
  (if just-shooted
      (append (search-for-ship (find-if #'(lambda (cell)
					    (let ((xc (first cell))
						  (yc (second cell))
						  (xj (first just-shooted))
						  (yj (second just-shooted)))
					      (and (or (= xc xj)
						       (= yc yj))
						   (or (= 1 (- xc xj))
						       (= 1 (- yc yj))))))
					(remove just-shooted killed-ships))
			       (remove just-shooted killed-ships))
	      (list just-shooted)
	      (search-for-ship (find-if #'(lambda (cell)
					    (let ((xc (first cell))
						  (yc (second cell))
						  (xj (first just-shooted))
						  (yj (second just-shooted)))
					      (and (or (= xc xj)
						       (= yc yj))
						   (or (= -1 (- xc xj))
						       (= -1 (- yc yj))))))
					(remove just-shooted killed-ships))
			       (remove just-shooted killed-ships)))))

(defun clever-random-killer (&key game-space-config ships-config)
  (declare (ignore ships-config))
  (let ((gsconfig game-space-config)
	killed-ships
	killed
	to-kill)
    #'(lambda (&optional result)
	(cond ((eql result :killed)
	       (push to-kill killed-ships)
	       (search-for-ship to-kill killed-ships)
	       (setf killed
		     (remove-duplicates (append killed 
						(aura-2d (list to-kill) 1)))))
	      ((eql result :shooted)
	       (push to-kill killed-ships)
	       (setf killed 
		     (remove-duplicates
		      (append killed (remove-if #'(lambda (cell)
						    (or (= (first cell)
							   (first to-kill))
							(= (second cell)
							   (second to-kill))))
						(aura-2d (list to-kill) 1)))))))
	(loop
	   (setf to-kill (list (+ 1 (random (first gsconfig)))
			       (+ 1 (random (second gsconfig)))))
	   (if (not (find to-kill killed :test #'equal))
	       (progn (push to-kill killed)
		      (return to-kill)))))))

;; Intelligent killer

;; (defclass hypotetic-ship (ship) ())

; (defmethod initialize-instanse :after ((ship hypotetic-ship) &key)
