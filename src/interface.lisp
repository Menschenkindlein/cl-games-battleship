(in-package :cl-games-battleship)

(defun loose (who)
  (format t "~a is looser!" who)
  (list :loose who))

(defun win (turn who)
  (format t "~a wins in ~a turn~:p" who turn)
  (list :win turn who))

(defun start-easy-game (&key
			(game-space-config '(10 10))
			(ships-config '(4
					3 3
					2 2 2
					1 1 1 1))
			(placer #'constant-placer)
			(killer-costructor #'constant-killer))
  (let* ((ships-positions (eval `(funcall ,placer
					  ',game-space-config
					  ',ships-config)))
	 (gamespace (make-instance 'game-space
				  :shconfig ships-config
				  :gsconfig game-space-config
				  :ships-positions
				  ships-positions))
	 (killer (eval `(funcall ,killer-costructor
				 :game-space-config ',game-space-config
				 :ships-config ',ships-config)))
	 (shooting-place (eval `(funcall ,killer)))
	 (result (shoot gamespace shooting-place)))
    (if (and (correct gamespace)
	     (= (length ships-positions)
		(length ships-config)))
	(let ((battle-result (loop
				for i upto (apply #'* game-space-config)
				doing
				  (if (not (find-ship-alive gamespace))
				      (return i))
				  (setf shooting-place
					(eval `(funcall ,killer ,result)))
				  (setf result (shoot gamespace shooting-place))
				finally (return :loose))))
	  (if (eql :loose battle-result)
	      (loose :killer)
	      (win battle-result :killer)))
	(loose :placer))))

(defun ask-human-for-shoot ()
  (format t "What is your place to shoot?~%")
  (list (read) (read)))

(defun ask-human-for-place ()
  (format t "Insert your ships position: ")
  (read))

(defun check-shooting-place (shooting-place game-space-config)
  (and (integerp (first shooting-place))
       (integerp (second shooting-place))
       (< 0 (first shooting-place))
       (>= (first game-space-config) (first shooting-place))
       (< 0 (second shooting-place))
       (>= (second game-space-config) (second shooting-place))))

(defun start-game-human (name
			 &key
			 (game-space-config '(10 10))
			 (ships-config '(4
					 3 3
					 2 2 2
					 1 1 1 1))
			 (placer #'constant-placer)
			 (killer-costructor #'constant-killer))
  (let* ((ships-positions-comp (eval `(funcall ,placer
					       ',game-space-config
					       ',ships-config)))
	 (gamespace-comp (make-instance 'game-space
					:shconfig ships-config
					:gsconfig game-space-config
					:ships-positions
					ships-positions-comp))
	 (ships-positions (ask-human-for-place))
	 (gamespace (make-instance 'game-space
				   :shconfig ships-config
				   :gsconfig game-space-config
				   :ships-positions
				   ships-positions))
	 (killer (eval `(funcall ,killer-costructor
				 :game-space-config ',game-space-config
				 :ships-config ',ships-config)))
	 shooting-place-comp result-comp)
    (cond
      ((or (not (correct gamespace))
	   (not (= (length ships-positions)
		   (length ships-config)))) (loose name))
      ((or (not (correct gamespace-comp))
	   (not (= (length ships-positions-comp)
		   (length ships-config)))) (loose :comp))
      (t
       (loop for turn from 1 upto (apply #'* game-space-config)
	  doing
	    (let
		((battle-result-human
		  (loop
		     (format t "This is your gamespace:~%")
		     (print-gamespace gamespace)
		     (format t "This is the comp's gamespace:~%")
		     (print-gamespace gamespace-comp t)
		     (if (not (find-ship-alive gamespace-comp))
			 (return (win turn name)))
		     (let ((shooting-place (ask-human-for-shoot))
			   result)
		       (if (check-shooting-place shooting-place
						 game-space-config)
			   (progn
			     (setf result (shoot gamespace-comp
						 shooting-place))
			     (cond
			       ((eql :missed result)
				(format t "~%You missed!~%~%")
				(sleep 2)
				(return))
			       ((eql :shooted result)
				(format
				 t "~%You have shooted the enemy boat!~%~%")
				(sleep 2))
			       ((eql :killed result)
				(format
				 t "~%You have killed the enemy boat!~%~%")
				(sleep 2))))
			   (return (loose name)))))))
	      (if battle-result-human
		  (return battle-result-human)
		  (let
		      ((battle-result-comp
			(loop 
			   (if (not (find-ship-alive gamespace))
			       (return (win turn :comp)))
			   (setf shooting-place-comp (funcall killer
							      result-comp))
			   (setf result-comp (shoot gamespace
						    shooting-place-comp))
			   (cond
			     ((eql :missed result-comp)
			      (progn
				(format t "~%Comp missed!~%~%")
				(sleep 2)
				(return)))
			     ((eql :shooted result-comp)
			      (format t "~%Comp have shooted your boat!~%~%")
			      (sleep 2))
			     ((eql :killed result-comp)
			      (format
			       t "~%Comp have killed your boat!~%~%")
			      (sleep 2))))))
		    (if battle-result-comp
			(return battle-result-comp))))))))))
