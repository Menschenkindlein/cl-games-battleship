(in-package :cl-games-battleship)

(defmethod print-game-space ((game-space game-space) &key enemy preview)
  (let ((answer (make-array (mapcar (lambda (x) (+ 2 x))
				    (gsconfig game-space))
			    :initial-element nil)))
    (loop for cell in (cube (sth-list (gsconfig game-space) 1)
			    (gsconfig game-space))
	 doing
	 (let ((gscell (find-cell game-space cell)))
	   (setf (apply #'aref answer cell)
		 (if (eql (class-of gscell) (find-class 'ship-cell))
		     (if (shooted gscell)
			 2                        ; <<<
			 (if enemy 
			     9                    ; <<<
			     1))                  ; <<<
		     (if (or (shooted gscell)
			     (and
			      preview
			      (find gscell
				    (apply #'append
					   (collect-the-lowest-level
					    (neighbours ship)
					    (ship in (ships
						      game-space)))))))
			 0                        ; <<<
			 9)))))                   ; <<<
    answer))

;; (defmacro print-game-space-for-sth (printed-game-space
;; 				    unknown sea ship shooted
;; 				    &rest divisors)
;;   `(let ((dimensions (array-dimensions ,printed-game-space)))
;;      (concatenate
;;       'string
;;       ,(car (last (first divisors)))
;;       (apply #'concatenate 'string
;; 	     (loop for cell in (cube dimensions
;; 				     (sth-list dimensions 0))
;; 		collecting
;; 		  (concatenate 'string
;; 			       ,@(loop for divisor in (reverse divisors)
;; 				      for n by 1
;; 				      collecting
;; 				      `(when (and (nth ,n cell)
;; 						  (= 0 (nth ,n cell))
;; 						  (not (every #'zerop cell)))
;; 					 (concatenate 'string ,@divisor)))
;; 			       (case (apply #'aref ,printed-game-space cell)
;; 				 (nil ,unknown)
;; 				 (0 ,sea)
;; 				 (1 ,ship)
;; 				 (2 ,shooted)))))
;;      ,(first (first divisors)))))

;; (defun print-game-space-for-web (printed-game-space)
;;   (print-game-space-for-sth printed-game-space
;; 			    "<td></td>" "<td class=\"sea\"></td>"
;; 			    "<td class=\"ship\"></td>"
;; 			    "<td class=\"shooted\"></td>"
;; 			    ("</table>" #(#\Newline) "<table>")
;; 			    ("</tr>" #(#\Newline) "<tr>") ("")))

;; (defun print-game-space-for-console (printed-game-space)
;;   (print-game-space-for-sth printed-game-space
;; 			    "   " " - "
;; 			    "[Q]" "bXd"
;; 			    ("" #(#\Newline) "")
;; 			    ("" #(#\Newline) "") ("")))
