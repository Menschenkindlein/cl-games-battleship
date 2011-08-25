(in-package :cl-games-battleship)

(defun constant-killer (&key game-space-config ships-config)
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
	to-kill)))

;; Intelligent killer

;; (defclass hypotetic-ship (ship) ())

; (defmethod initialize-instanse :after ((ship hypotetic-ship) &key)
