(in-package :cl-games-battleship)

(defclass player ()
  ((name :initarg :name
	 :reader name)
   (player :initarg :player
	   :reader player)
   (config :initarg :config
	   :reader config)
   (player-class :initarg :player-class
		 :accessor player-class)))

(defclass placer (player) ())

(defclass killer (player)
  ((player :accessor player)
   (killing-sequence :accessor killing-sequence)))

(defun output (content &key (type :text))
  (case *printing*
    (:console (case type
		(:text (format t content))
		(:game-space (print-game-space-for-console
			      printed-game-space))))))

(defun ask-human-shoot (printed-game-space)
  (print-game-space-for-console printed-game-space)
  (format t "What is your place to shoot?~%")
  (finish-output)
  (reverse (loop repeat (length (array-dimensions printed-game-space))
	      collecting (read))))

(defmethod initialize-instance :after ((killer killer) &key)
  (if (eql :human (player killer))
      (ask-human-shoot (make-array (first (config killer))
				  :initial-element nil))
      (let ((killer-construct (funcall (player killer)
				       (first (config killer))
				       (second (config killer)))))
	(setf (killing-sequence killer) (first killer-construct))
	(setf (player killer) (second killer-construct)))))

(defgeneric ask (player))

(defun ask-human-place (game-space-config ships-config)
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
		    (print-game-space-for-console (print-game-space
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

(defmethod ask ((placer placer))
  (let ((gsconfig (first (config placer)))
	(ships-config (second (config placer))))
    (if (eq :human (player placer))
	(ask-human-place gsconfig ships-config)
	(let ((ships-positions
	       (funcall (player placer)
			gsconfig
			ships-config)))
	  (if (and ships-positions
		   (listp ships-positions)
		   (= (length ships-positions) (length ships-config))
		   (every (lambda (ship) (integerp (first ship)))
			  ships-positions)
		   (every (lambda (ship) (every #'integerp (second ship)))
			  ships-positions)
		   (every (lambda (ship) (integerp (third ship)))
			  ships-positions)
		   (every (lambda (sh pl) (= sh (first pl)))
			  ships-config ships-positions)
		   (every (lambda (ship) (>= (length gsconfig)
					     (third ship)))
			  ships-positions))
	      ships-positions)))))

(defmethod ask ((killer killer))
  (let ((place-to-shoot (pop (killing-sequence killer))))
    (if (and
	 place-to-shoot
	 (listp place-to-shoot)
	 (every #'integerp place-to-shoot)
	 (= (length place-to-shoot) (length (gsconfig killer)))
	 (every #'plusp place-to-shoot)
	 (every #'< place-to-shoot (gsconfig killer)))
	place-to-shoot)))

(defgeneric change-killing-sequence (killer result printed-game-space))

(defmethod change-killing-sequence ((killer killer) result printed-game-space)
  (when (eql :human (first killing-sequence))
    (setf (player killer) (pop killing-sequence)))
  (if (eql :human (player killer))
      (let ((human-answer (ask-human-kill printed-game-space)))
	(setf (killing-sequence killer) (first human-answer))
	(unless (eql :human (second (first human-answer)))
	  (setf (player killer) (second human-answer)))))
      (if (player killer)
	  (setf (killing-sequence killer)
	      (let ((killing-sequence
		     (funcall
		      (funcall (player killer)
			       result)
		      (killing-sequence killer))))
		(if (and killing-sequence
			 (listp killing-sequence))
		    killing-sequence)))))

(defclass game ()
  ((game-space :accessor game-space)
   (config :initarg :config
	   :initform (list (list 10 10)
			   (list 4 3 3 2 2 2 1 1 1 1))
	   :reader config)
   (result :accessor result)
   (placer :accessor placer)
   (killer :killer killer)))

(defmethod initialize-instance :after ((game game)
				       &key
				       placer
				       killer)
  ;; Initialize built-in bots
  (when (integrp placer)
    (setf (placer game) (case placer
			  (1 (list :built-in-bot :easy #'constant-placer))
			  (2 (list :built-in-bot :hard #'random-placer-bf)))))
  (when (integrp killer)
    (setf (killer game) (case killer
			  (1 (list :built-in-bot :easy #'constant-killer))
			  (2 (list :built-in-bot :hard #'random-killer)))))
  (if (not (test-for-correctness (config game)))
      (setf (result game) :gconfig-wrong)
      (progn
	;; Initialize gamespace by the placer call
	(setf (placer game) (make-instance 'placer
					   :player-class
					   (cond
					     ((eql :built-in-bot
						    (first placer))
					      (pop placer))
					     ((eql :human (second placer))
					      :human)
					     (t :bot))
					   :name (first placer)
					   :config (config game)
					   :player (second placer)))
	(let ((ships-positions (ask (placer game) config)))
	  (unless (and ships-positions
		       (correct
			(setf (game-space game)
			      (make-instance 'game-space
					     :ships-positions ships-positions
					     :gsconfig gsconfig))))
	    (setf (result game) :placer-wrong)))
	;; Initialize killing-sequence by constructor-call
	(setf (killer game) (make-instance 'killer
					   :player-class
					   (cond
					     ((eql :built-in-bot
						    (first killer))
					      (pop killer))
					     ((eql :human (second killer))
					      :human)
					     (t :bot))
					   :name (first killer)
					   :config (config game)
					   :player (second killer)))
	(unless (killing-sequence killer)
	  (setf (result game) :killer-wrong)))))

(defgeneric turn (game))

(defmethod turn ((game game) first)
  (unless first
    (change-killing-sequence (result game)
			     (print-game-space
			      (game-space game))))
  (or (unless (killing-sequence (killer game))
	(setf (result game) :killer-wrong))
      (let ((shooting-place (ask (killer game))))
	(or (unless shooting-place
	      (setf (result game) :killer-wrong))
	    (let ((shooting-result
		   (shoot (game-space game) shooting-place)))
	      (setf (result game) 
		    (if (cleared (game-space game))
			:killer-wins
			shooting-result)))))))

(defun begin-big-game (placer killer config)
  (let ((game (make-instance 'game
			     :config config
			     :placer placer
			     :killer killer)))
    (loop while (find (result game)
		      '(:missed :shooted :killed nil))
	 doing
	 (turn game))
    (result game)))

(defun begin-great-game (first-player-pair second-player-pair
			 game-config *printing* &optional second-game-config)
  (declare (special *printing*))
  (let ((first-game (make-instance 'game
				   :placer (first first-player-pair)
				   :killer (second second-player-pair)
				   :config game-config))
	(second-game (make-instance 'game
				    :placer (second first-player-pair)
				    :killer (first second-player-pair)
				    :config (or second-game-config
						game-config)))
	result-of-the-game)
    (if (or (result first-game) (result second-game))
	(setf result-of-the-game
	      (list
	       (case (result first-game)
		 (:placer-wrong :first-placer-wrong)
		 (:gconfig-wrong :first-gconfig-wrong))
	       (case (result second-game)
		 (:placer-wrong :second-placer-wrong)
		 (:gconfig-wrong :second-gconfig-wrong))))
	(setf result-of-the-game
	      (loop named bigloop for turn from 1 by 1 doing
		 (loop
		    (case (turn first-game)
		      (:shooted nil)
		      (:killed nil)
		      (:missed (return nil))
		      (:killer-wrong (return-from bigloop
				       :second-killer-wrong))))
		 (loop
		    (case (turn second-game)
		      (:shooted nil)
		      (:killed nil)
		      (:missed (return nil))
		      (:killer-wrong (return-from bigloop
				       :first-killer-wrong)))))))
    (case *printing*
      (nil result-of-the-game))))
