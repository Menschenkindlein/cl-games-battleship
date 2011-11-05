(in-package :cl-games-battleship)

(defun loose (who)
  (format t "~%~a is looser!" who)
  (list :loose who))

(defun win (turn who)
  (format t "~%~a wins in ~a turn~:p!" who turn)
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
  (if (eql killer-constructor :strategic)
      (setf killer-constructor #'strategic-random-killer))
  (let* ((ships-positions (eval `(funcall ,placer
					  ',game-space-config
					  ',ships-config)))
	 (gamespace (make-instance 'game-space
				   :gsconfig game-space-config
				   :ships-positions
				   ships-positions))
	 (killer (eval `(funcall ,killer-constructor
				 ',game-space-config
				 ',ships-config))))
    (print-gamespace-console gamespace :preview t)
    (let* ((shooting-place (eval `(funcall ,killer)))
	   (result (shoot gamespace shooting-place)))
      (if (and (correct gamespace)
	       (= (length ships-positions)
		  (length ships-config)))
	  (let ((battle-result (loop
				  for i upto (apply #'* game-space-config)
				  doing
				    (if (cleared gamespace)
					(return i))
				    (setf shooting-place
					  (eval `(funcall ,killer ,result)))
				    (setf result (shoot gamespace
							shooting-place)))))
	    (if battle-result
	        (win (+ 1 battle-result) :killer)
		(loose :killer)))
	  (loose :placer)))))

(defun ask-human-for-shoot (config)
  (format t "What is your place to shoot?~%")
  (reverse (loop for coord in config collecting (read))))

(defun ask-human-for-place (ships-config game-space-config)
  (format t "What method of inserting your ships position do you prefere?~%")
  (format t "1) Step by step;~%2) All together;~%")
  (format t "3) Let random-bruteforce placer place my ships.~%")
  (format t "Choose 1, 2 or 3: ")
  (finish-output)
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
		    (print-gamespace-console
		      (make-instance 'game-space
				     :gsconfig game-space-config
				     :ships-positions
				     (if position
					 position
					 (list 
					  (list 1 (make-list
						   (length game-space-config)
						   :initial-element -1) 1))))
		      :preview t)
		    (format t "~a ship~:p left.~%" left)
		    (format t "Insert your ~a-deck ship position (to clear gamespace insert :clear among coords): " ship)
		    (finish-output)
		    (setf pos (loop for coord in game-space-config
				 collecting (read)))
		    (when (find :clear pos)
		      (push (list 1 (make-list (length game-space-config)
					       :initial-element -1) 1)
						  position) (return))
		    (when (find-if-not #'integerp pos)
		      (error "Wrong answer!"))
		    (if (and (< 1 ship) (< 1 (length game-space-config)))
			(progn
			  (format t "Insert your ~a-deck ship orientation: 1) gorisontal; 2) vertical; etc:" ship)
			  (finish-output)
			  (setf pos (list ship pos (read))))
			(setf pos (list ship pos 1)))
		    (push pos position)
		    (if (correct (make-instance 'game-space
						:gsconfig game-space-config
						:ships-positions position))
			(return)
			(progn
			  (pop position)
			  (format
			   t "This is a wrong position.  Try again.~%")))))
	    (let ((gamespace (make-instance 'game-space
					    :gsconfig game-space-config
					    :ships-positions position)))
	      (when (correct gamespace)
		(format t "Is this an ultimate variant? (:y/:n) ")
		(finish-output)
		(if (eql (read) :y)
		    (return position)))))))
      
      ((= 2 choose)
       (loop
	  (format t "Insert your ships position:~%")
	  (let ((position (read)))
	    (if (not (and (listp position)
			  (= (length position) (length ships-config))
			  (equal ships-config (mapcar #'car position))))
		(format t "This is a wrong position.  Try again.~%")
		(let ((gamespace (make-instance 'game-space
						:gsconfig game-space-config
						:ships-positions position)))
		  (if (correct gamespace)
		      (progn
			(format t "Is this an ultimate variant? (:y/:n) ")
			(finish-output)
			(if (eql (read) :y)
			    (return position)))
		      (format t "This is a wrong position.  Try again.~%")))))))
      ((= 3 choose) (random-placer-bf game-space-config ships-config))
      (t (error "Wrong answer!")))))

(defun check-shooting-place (shooting-place game-space-config)
  (not (find-if-not #'null (mapcar (lambda (point conf) (or (not
							     (integerp point))
							    (> 1 point)
							    (< conf point)))
				   shooting-place game-space-config))))

(defun start-game-human (name
			 &key
			 (game-space-config '(10 10))
			 (ships-config '(4
					 3 3
					 2 2 2
					 1 1 1 1))
			 (placer #'random-placer-bf)
			 (killer-constructor #'random-killer))
  (if (eql placer :constant) (setf placer #'constant-placer))
  (if (eql killer-constructor :constant)
      (setf killer-constructor #'constant-killer))
  (let* ((ships-positions-comp (eval `(funcall ,placer
					       ',game-space-config
					       ',ships-config)))
	 (gamespace-comp (make-instance 'game-space
					:gsconfig game-space-config
					:ships-positions
					ships-positions-comp))
	 (ships-positions (ask-human-for-place ships-config
					       game-space-config))
	 (gamespace (make-instance 'game-space
				   :gsconfig game-space-config
				   :ships-positions
				   ships-positions))
	 (killer (eval `(funcall ,killer-constructor
				 ',game-space-config
				 ',ships-config)))
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
		     (print-gamespace-console gamespace)
		     (format t "This is the comp's gamespace:~%")
		     (print-gamespace-console gamespace-comp :enemy t)
		     (if (cleared gamespace-comp)
			 (return (win turn name)))
		     (let ((shooting-place (ask-human-for-shoot
					    game-space-config))
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
			   (if (cleared gamespace)
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
