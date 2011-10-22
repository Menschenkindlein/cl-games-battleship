(in-package :cl-games-battleship)

(defclass real-thing ()
  ((neighbours :initarg :neighbours :accessor neighbours)))

(defclass cell ()
  ((shooted :initform nil
	    :accessor shooted)))

(defclass sea-cell (cell) ())

(defclass ship-cell (real-thing cell) 
  ((ship :initarg :ship :reader ship-of)))

(defclass multy-cell-object ()
  ((own-cells :accessor own-cells)
   (correct :initform t
	    :accessor correct)))

(defclass ship (multy-cell-object real-thing)
  ((alive :initform t
	  :accessor alive)))

(defclass game-space (multy-cell-object)
  ((game-space-config :initarg :gsconfig
		      :reader gsconfig)
   (ships :accessor ships)))

(defgeneric find-cell (game-space where))

(defmethod find-cell ((game-space game-space) cell)
  (apply #'aref (own-cells game-space) cell))

(defun (setf find-cell) (value game-space cell)
  (setf (apply #'aref (own-cells game-space) cell) value))

(defmethod initialize-instance :after ((game-space game-space)
				       &key ships-positions)
  (setf (own-cells game-space)
	(make-array (mapcar (lambda (x) (+ 2 x)) (gsconfig game-space))
		    :initial-element nil))
  (setf (ships game-space)
	(loop for ship in ships-positions
	   collecting
	     (let ((ship-object
		    (make-instance 'ship
				   :size (first ship)
				   :coords (second ship)
				   :direction (- (third ship) 1)
				   :game-space game-space)))
	       (if (null (correct ship-object))
		   (return (setf (correct game-space) nil))
		   ship-object))))
  (when (correct game-space)
    (loop for cell in (cube (sth-list (gsconfig game-space) 1)
			    (gsconfig game-space))
       doing
	 (unless (find-cell game-space cell)
	   (setf (find-cell game-space cell) (make-instance 'sea-cell))))))

(defun perforated-sphere (center)
  (remove-if #'(lambda (x)
		 (= 1 (count-if #'null
			        (mapcar #'= x center))))
	     (sphere center 1)))

(defmethod initialize-instance :after ((ship ship)
				       &key coords size direction game-space)
  (let ((own-cells-indexes
	 (loop for inc upto (- size 1)
	   collecting
	     (let ((coords (copy-list coords)))
	       (incf (nth direction coords) inc)
	       (if (or (eql (class-of (find-cell game-space coords))
			    (find-class 'ship-cell))
		       (find-if-not #'null
				    (mapcar
				     (lambda (x g)
				       (or (< g x)
					   (> 1 x)))
				     coords (gsconfig game-space))))
		   (return (setf (correct ship) nil))
		   coords)))))
    (when (correct ship)
      (setf (own-cells ship)
	    (loop for coords in own-cells-indexes collecting
		 (setf (find-cell game-space coords)
		       (make-instance
			'ship-cell
			:ship ship
			:neighbours 
			(loop for ncell
			   in (perforated-sphere coords)
			   collecting
			     (or (find-cell game-space ncell)
				 (setf (find-cell game-space ncell)
				       (make-instance 'sea-cell))))))))
      (setf (neighbours ship)
	    (loop for cell in
		 (aura own-cells-indexes 1)
	       collecting
		 (if (eql (class-of (find-cell game-space cell))
			  (find-class 'ship-cell))
		     (return (setf (correct ship) nil))
		     (or (find-cell game-space cell)
			 (setf (find-cell game-space cell)
			       (make-instance 'sea-cell)))))))))

(defgeneric shoot (game-space where))

(defmethod shoot ((game-space game-space) where)
  (kill (find-cell game-space where)))

(defgeneric kill (object))

(defmethod kill :before ((cell cell))
  (unless (shooted cell)
    (setf (shooted cell) t)))

(defmethod kill ((cell sea-cell))
  :missed)

(defgeneric drowning (ship))

(defmethod drowning ((ship ship))
  (not (find-if-not #'shooted (own-cells ship))))

(defmethod kill :before ((real-thing real-thing))
  (mapcar #'kill (neighbours real-thing)))

(defmethod kill ((cell ship-cell))
  (if (drowning (ship-of cell))
      (kill (ship-of cell))
      :shooted))

(defmethod kill ((ship ship))
  (setf (alive ship) nil)
  :killed)

(defgeneric cleared (game-space))

(defmethod cleared ((game-space game-space))
  (not (find-if (lambda (ship) (alive ship))
		(ships game-space))))

