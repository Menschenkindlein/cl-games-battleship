(in-package :cl-games-battleship)

(defun constant-killer (&key game-space-config ships-config result)
  (if game-space-config
      (progn
	(setf *killer-stack* (list '(1 1) game-space-config))
	(first *killer-stack*))
      (let ((to-kill (first *killer-stack*))
	    (gsconfig (second *killer-stack*)))
	(if (= (first gsconfig)
	       (first to-kill))
	(setf *killer-stack* (list (list 1 (+ 1 (second to-kill))) gsconfig))
	(setf *killer-stack* (list (list (+ 1 (first to-kill))
					 (second to-kill)) gsconfig)))
	(if (>= (second gsconfig) (second to-kill))
	    (first *killer-stack*)))))

;; Intelligent killer

;; (defclass hypotetic-ship (ship) ())

; (defmethod initialize-instanse :after ((ship hypotetic-ship) &key)
