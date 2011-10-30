(in-package :cl-games-battleship)

(defun print-level (gamespace divisors &key (address nil) enemy preview)
  (let ((sizes (subseq (reverse (mapcar (lambda (x) (- x 2))
					(array-dimensions gamespace)))
		       (length address))))
    (if (> (length divisors) (length sizes))
        (setf divisors (subseq divisors (- (length divisors)
					   (length sizes)))))
    (if sizes
	(loop for i from 1 upto (car sizes) doing
	     (if (= (length sizes) (length divisors))
		 (progn
		   (funcall (car divisors) sizes i)
		   (print-level gamespace (cdr divisors)
				:address (cons i address)
				:enemy enemy :preview preview))	
		 (progn
		   (format t "~%~%It is ~a-D level number ~a:~%"
			   (length (cdr sizes)) i)
		   (print-level gamespace divisors :address (cons i address)
				:enemy enemy :preview preview))))
	(let ((cell (apply #'aref gamespace address)))
	  (case cell
	    (1   (princ "[Q]"))
	    (2   (princ "bXd"))
	    (0   (princ " - "))
	    (9   (princ "   ")))))))

(defun print-gamespace-console (gamespace &key enemy preview)
  (let* ((gamespace (print-game-space gamespace :enemy enemy :preview preview))
	 (dimensions (mapcar (lambda (x) (- x 2)) (array-dimensions gamespace)))
	 (divisors 
	  (list
	   (lambda (sizes i)
	     (unless (= 0 i)
	       (format t "~%~%It is ~a-D level number ~a:~%"
		       (length (cdr sizes)) i))
	     (princ "    ")
	     (format t "~{~2d ~}"
		     (loop for x from 1 upto (car (last sizes)) collecting x)))
	   (lambda (sizes i) (declare (ignore sizes))
		   (format t "~%~2d |" i))
	   (lambda (sizes i) (declare (ignore sizes i))))))
    (format t "~%~%It is ~a-D gamespace:~%" (length
					     dimensions))
    (if (< (length dimensions) (length divisors))
	(funcall (nth (- (length divisors)
			 (length dimensions) 1)
		      divisors)
		 dimensions 0))
    (print-level gamespace divisors :enemy enemy :preview preview)
    (princ #\Newline)))
