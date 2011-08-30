(in-package :cl-games-battleship)

(defun loose (who)
  (format t "~a is looser!" who)
  (list :loose who))

(defun win (turn who)
  (format t "~a wins in ~a turn~:p!" who turn)
  (list :win turn who))

(defun start-easy-game (&key
			(game-space-config '(10 10))
			(ships-config '(4
					3 3
					2 2 2
					1 1 1 1))
			(placer #'constant-placer)
			(killer-constructor #'constant-killer))
  (if (eql placer :random-bf) (setf placer #'random-placer-bf))
  (if (eql killer-constructor :random)
      (setf killer-constructor #'random-killer))
  (if (eql killer-constructor :random-cl)
      (setf killer-constructor #'clever-random-killer))
  (let* ((ships-positions (eval `(funcall ,placer
					  ',game-space-config
					  ',ships-config)))
	 (gamespace (make-instance 'game-space
				  :shconfig ships-config
				  :gsconfig game-space-config
				  :ships-positions
				  ships-positions))
	 (killer (eval `(funcall ,killer-constructor
				 :game-space-config ',game-space-config
				 :ships-config ',ships-config))))
    (print-gamespace gamespace)
    (let* ((shooting-place (eval `(funcall ,killer)))
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
				    (setf result (shoot gamespace
							shooting-place))
				  finally (return :loose))))
	    (if (eql :loose battle-result)
		(loose :killer)
		(win (+ 1 battle-result) :killer)))
	  (loose :placer)))))

(defun ask-human-for-shoot ()
  (format t "What is your place to shoot?~%")
  (list (read) (read)))

(defun ask-human-for-place (ships-config game-space-config)
  (format t "What method of inserting your ships position do you prefere?~%")
  (format t "1) Step by step;~%2) All together.~%Choose 1 or 2: ")
  (let ((choose (read)))
    (cond
      ((= 1 choose)
       (loop
	  (let (position pos)
	    (loop
	       for ship in ships-config
	       for left downfrom (length ships-config)
	       doing
		 (loop
		    (format t "This is your current position:~%")
		    (print-gamespace
		     (make-instance 'game-space
				    :shconfig ships-config
				    :gsconfig game-space-config
				    :ships-positions (if position
							 position
							 '((1 (-1 -1) nil)))))
		    (format t "~a ship~:p left.~%" left)
		    (format t "Insert your ~a-deck ship position: "
			    ship)
		    (setf pos (list (read) (read)))
		    (when (not (and (integerp (first pos))
				    (integerp (second pos))))
		      (error "Wrong answer!"))
		    (if (< 1 ship)
			(progn
			  (format
			   t "Insert your ~a-deck ship orientation (t/nil): "
			   ship)
			  (setf pos (list ship pos (read))))
			  (setf pos (list ship pos nil)))
		    (push pos position)
		    (if (correct (make-instance 'game-space
						:shconfig ships-config
						:gsconfig game-space-config
						:ships-positions position))
			(return)
			(progn
			  (pop position)
			  (format
			   t "This is a wrong position.  Try again.~%")))))
	    (let ((gamespace (make-instance 'game-space
					    :shconfig ships-config
					    :gsconfig game-space-config
					    :ships-positions position)))
	      (if (correct gamespace)
		  (progn
		    (format t "Is this an ultimate variant? (:y/:n) ")
		    (if (eql (read) :y)
			(return position)))
		  (format t "This is a wrong position.  Try again.~%"))))))
       
      ((= 2 choose)
       (loop
	  (format t "Insert your ships position:~%")
	  (let ((position (read)))
	    (if (not (and (listp position)
			  (= (length position) (length ships-config))
			  (equal ships-config (mapcar #'car position))))
		(format t "This is a wrong position.  Try again.~%")
		(let ((gamespace (make-instance 'game-space
						:shconfig ships-config
						:gsconfig game-space-config
						:ships-positions position)))
		  (if (correct gamespace)
		      (progn
			(format t "Is this an ultimate variant? (:y/:n) ")
			(if (eql (read) :y)
			    (return position)))
		      (format t "This is a wrong position.  Try again.~%")))))))
      (t (error "Wrong answer!")))))

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
			 (placer #'random-placer-bf)
			 (killer-constructor #'clever-random-killer))
  (if (eql placer :constant) (setf placer #'constant-placer))
  (if (eql killer-constructor :constant)
      (setf killer-constructor #'constant-killer))
  (if (eql killer-constructor :random)
      (setf killer-constructor #'random-killer))
  (let* ((ships-positions-comp (eval `(funcall ,placer
					       ',game-space-config
					       ',ships-config)))
	 (gamespace-comp (make-instance 'game-space
					:shconfig ships-config
					:gsconfig game-space-config
					:ships-positions
					ships-positions-comp))
	 (ships-positions (ask-human-for-place ships-config
					       game-space-config))
	 (gamespace (make-instance 'game-space
				   :shconfig ships-config
				   :gsconfig game-space-config
				   :ships-positions
				   ships-positions))
	 (killer (eval `(funcall ,killer-constructor
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
