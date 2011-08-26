(in-package :cl-games-battleship)

(defun print-gamespace (gamespace &optional enemy)
  (princ "    ")
  (loop for y from 1 upto (second (gsconfig gamespace)) doing
       (format t " ~:[~a ~;~a~]" (< 9 y) y))
  (princ #\Newline)
  (loop for x from 1 upto (first (gsconfig gamespace)) doing
       (format t "~:[ ~a~;~a~] |" (< 9 x) x)
       (loop for y from 1 upto (second (gsconfig gamespace)) doing
	    (let ((ship (find-a-ship gamespace (list (- x 1)
						     (- y 1))))
		  (sea-cell (find-cell (sea gamespace) (list
							(- x 1)
							(- y 1)))))
	      (if ship
	       (if (shooted (find-cell ship (list (- x 1)
						  (- y 1))))
		   (princ "bXd")
		   (princ (if enemy "   " "[Q]")))
	       (if (shooted sea-cell)
		   (princ " - ")
		   (princ "   ")))))
       (format t "|~%")))