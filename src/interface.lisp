(in-package :cl-games-battleship)

(defun loose ()
  (princ "Killer is looser!")
  (list :loose))

(defun win (turn)
  (format t "Killer wins in ~a turns" turn)
  (list :win turn))

(defun start-easy-game (game-space-config
			ships-config
			&key
			(placer #'constant-placer)
			(killer-costructor #'constant-killer))
  (let ((gamespace (make-instance 'game-space
				  :shconfig ships-config
				  :gsconfig game-space-config
				  :placer placer)))
    (let* ((killer (funcall killer-costructor
			    :game-space-config game-space-config
			    :ships-config ships-config))
	   (shooting-place (funcall killer))
	   (result (shoot gamespace shooting-place)))
      (let ((battle-result (loop
			      for i upto (apply #'* game-space-config)
			      doing
				(setf shooting-place
				      (funcall killer result))
				(setf result (shoot gamespace shooting-place))
				(if (not (find-ship-alive gamespace))
				    (return i))
			      finally (return :loose))))
	(if (eql :loose battle-result)
	    (loose)
	    (win battle-result))))))

;; ;; Using example
;; (start-easy-game '(10 10)
;; 	            '(4 3 3 2 2 2 1 1 1 1)
;; 	            :placer #'constant-placer
;; 	            :killer #'constant-killer)