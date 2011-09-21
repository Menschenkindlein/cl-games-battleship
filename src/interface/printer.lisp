(in-package :cl-games-battleship)

(defun print-level (gamespace divisors &key (address nil) enemy)
  (let ((sizes (subseq (reverse (gsconfig gamespace)) (length address))))
    (if (> (length divisors) (length sizes))
        (setf divisors (subseq divisors (- (length divisors)
					   (length sizes)))))
    (if sizes
	(loop for i from 1 upto (car sizes) doing
	     (if (= (length sizes) (length divisors))
		 (progn
		   (funcall (car divisors) sizes i)
		   (print-level gamespace (cdr divisors) :address (cons i address) :enemy enemy))	
		 (progn
		   (format t "~%~%It is ~a-D level number ~a:~%" (length (cdr sizes)) i)
		   (print-level gamespace divisors :address (cons i address) :enemy enemy))))
	(let ((cell (find-cell gamespace address)))
	  (if (eql (class-of cell) (find-class 'ship-cell))
	      (if (shooted cell)
		  (princ "bXd")
		  (princ (if enemy "   " "[Q]")))
	      (if (shooted cell)
		  (princ "(O)")
		  (princ "   ")))))))

(defun print-gamespace (gamespace &optional enemy)
  (let ((divisors 
	 (list
	  (lambda (sizes i)
	    (unless (= 0 i) (format t "~%~%It is ~a-D level number ~a:~%" (length (cdr sizes)) i))
	    (princ "    ")
	    (format t "~{~2d ~}" (loop for x from 1 upto (car (last sizes)) collecting x)))
	  (lambda (sizes i) (declare (ignore sizes))
		  (format t "~%~2d |" i))
	  (lambda (sizes i) (declare (ignore sizes i))))))
    (format t "~%~%It is ~a-D gamespace:~%" (length (gsconfig gamespace)))
    (if (< (length (gsconfig gamespace)) (length divisors))
	(funcall (nth (- (length divisors)
			 (length (gsconfig gamespace)) 1)
		      divisors)
		 (gsconfig gamespace) 0))
    (print-level gamespace divisors :enemy enemy) (princ #\Newline)))
