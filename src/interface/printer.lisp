(in-package :cl-games-battleship)

(defmethod print-game-space ((game-space game-space) &key enemy preview)
  (let ((answer (make-array (gsconfig game-space))))
    (loop for cell in (cube (gsconfig game-space)
			    (sthlist (gsconfig game-space) 1))
	 doing
	 (let ((gscell (find-cell game-space cell)))
	   (setf (apply #'aref answer (mapcar #1- cell))
		 (if (eql (class-of cell) (find-class 'ship-cell))
		     (if (shooted cell)
			 2                        ; <<<
			 (if enemy 
			     nil                  ; <<<
			     1))                  ; <<<
		     (if (or (shooted cell)
			     (and
			      preview
			      (find cell
				    (apply #'append
					   (collect-the-lowest-level
					    (neighbours ship)
					    (ship in (ships
						      gamespace)))))))
			 0                        ; <<<
			 nil)))))))               ; <<<

(defmacro print-game-space-for-sth (printed-game-space
				    unknown sea ship shooted
				    &rest divisors)
  `(let ((dimensions (array-dimensions ,printed-game-space)))
     (concatenate
      'string
      ,(car (last (first divisors)))
      (apply #'concatenate 'string
	     (loop for cell in (cube dimensions
				     (sth-list dimensions 0))
		collecting
		  (concatenate 'string
			       ,@(loop for divisor in (reverse divisors)
				      for n by 1
				      collecting
				      `(when (and (nth ,n cell)
						  (= 0 (nth ,n cell))
						  (not (every #'zerop cell)))
					 (concatenate 'string ,@divisor)))
			       (case (apply #'aref ,printed-game-space cell)
				 (nil ,unknown)
				 (0 ,sea)
				 (1 ,ship)
				 (2 ,shooted)))))
     ,(first (first divisors)))))

(defun print-game-space-for-web (printed-game-space)
  (print-game-space-for-sth printed-game-space
			    "<td></td>" "<td class=\"sea\"></td>"
			    "<td class=\"ship\"></td>"
			    "<td class=\"shooted\"></td>"
			    ("</table>" #(#\Newline) "<table>")
			    ("</tr>" #(#\Newline) "<tr>") ("")))

(defun print-game-space-for-console (printed-game-space)
  (print-game-space-for-sth printed-game-space
			    "   " " - "
			    "[Q]" "bXd"
			    ("" #(#\Newline) "")
			    ("" #(#\Newline) "") ("")))
