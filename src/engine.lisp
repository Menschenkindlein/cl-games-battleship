(in-package :cl-games-battleship)

(defclass cell ()
  ((coordinates :initarg :coords
		:reader coords)
   (shooted :initform nil
	    :accessor shooted)))

(defclass sea-cell (cell) ())

(defclass ship-cell (cell) ())

(defgeneric shoot-cell (cell))

(defmethod shoot-cell ((cell cell))
  (setf (shooted cell) t))

(defclass multy-cell-object ()
  ((own-cells :accessor own-cells)))

(defgeneric find-cell (object where))

(defmethod find-cell ((object multy-cell-object) where)
  (find where (own-cells object)
	:test #'(lambda (where cell)
		  (equal where (coords cell)))))

(defclass sea (multy-cell-object) ())

(defmethod initialize-instance :after ((sea sea) &key gsconfig ships)
  (setf (own-cells sea)
	(remove-if #'(lambda (cell)
		       (find-if #'(lambda (ship)
				    (find-cell ship (coords cell)))
				ships))
		   (mapcar (lambda (cell)
				(make-instance 'sea-cell :coords cell))
			   (cube (sth-list gsconfig 0)
				 (mapcar (lambda (x) (+ 2 x)) gsconfig))))))

(defclass ship (multy-cell-object)
  ((coordinates :initarg :coords
		:reader coords)
   (size :initarg :size
	 :reader size)
   (direction :initarg :direction
	      :reader direction)
   (alive :initform t
	  :accessor alive)))

(defclass real-ship (ship) ;; The difference is neaded for killer AI
   ((neighbours :accessor neighbours)))

(defmethod initialize-instance :after ((ship ship) &key)
  (with-accessors ((coords coords)
		   (size size)
		   (direction direction)) ship
      (setf (own-cells ship)
	    (loop for inc upto (- size 1)
	       collecting
		(let ((coords (loop for coord in coords collecting coord)))
		  (incf (nth direction coords) inc)
		  (make-instance 'ship-cell
				 :coords coords))))))

(defmethod initialize-instance :after ((ship real-ship) &key)
  (let ((own-cells (loop for cell in (own-cells ship)
			collecting (coords cell))))
    (setf (neighbours ship)
	  (aura own-cells 1))))

(defgeneric shoot (object where))

(defmethod shoot ((object sea) where)
  (shoot-cell (find-cell object where))
  :missed)

(defgeneric shoot-ship (ship sea where))

(defun perforated-sphere (center)
	(remove-if #'(lambda (x)
	                 (= 1 (count-if #'null
			        (mapcar #'= x center))))
			(sphere center 1)))

(defmethod shoot-ship ((ship real-ship) (sea sea) where)
  (shoot-cell (find-cell ship where))
  (if (not (find-if-not #'(lambda (cell)
			    (shooted cell))
			(own-cells ship)))
      (progn
	(setf (alive ship) nil)
	(loop for cell in (neighbours ship) doing
	     (shoot sea cell))
	:killed)
      (progn
	(loop for cell in
	     (perforated-sphere where)
	   doing
	     (shoot sea cell))
	:shooted)))

(defclass game-space ()
  ((game-space-config :initarg :gsconfig
		      :reader gsconfig)
   (ships :accessor ships)
   (sea :accessor sea)
   (correct :accessor correct)))

(defgeneric correct (game-space))

(defmethod correct ((game-space game-space))
  (let ((all-own-cells (mapcar #'(lambda (cell)
				   (coords cell))
			       (apply #'append
				      (collect-the-lowest-level
				       (own-cells ship)
				       (ship in (ships game-space))))))
	(all-nearest-cells (apply #'append
				  (collect-the-lowest-level
				   (neighbours ship)
				   (ship in (ships game-space))))))
    (loop for cell in all-own-cells never
		   (or (find cell all-nearest-cells :test #'equal)
		       (< 1 (count cell all-own-cells :test #'equal))
		       (find-if (lambda (x) (> 1 x)) cell)
		       (find-if-not #'null (mapcar #'< (gsconfig game-space) cell))))))

(defmethod initialize-instance :after ((game-space game-space)
				       &key ships-positions)
  (setf (ships game-space)
	(loop for ship in ships-positions
	   collecting
	     (make-instance 'real-ship
			    ;; Converting numbers into indexes
			    :size (first ship)
			    :coords (second ship)
			    :direction (- (third ship) 1))))
  (setf (sea game-space) (make-instance 'sea
					:gsconfig (gsconfig game-space)
					:ships (ships game-space))))

(defgeneric find-a-ship (game-space where))

(defmethod find-a-ship ((game-space game-space) where)
  (find-if #'(lambda (ship)
	       (find-cell ship where)) (ships game-space)))

(defgeneric find-ship-alive (game-space))

(defmethod find-ship-alive ((game-space game-space))
  (find-if #'(lambda (ship)
	       (if (alive ship)
		   ship)) (ships game-space)))

(defmethod shoot ((game-space game-space) where)
    (if (find-a-ship game-space where)
	(shoot-ship (find-a-ship game-space where) (sea game-space) where)
	(shoot (sea game-space) where)))

(defmethod find-cell ((object game-space) where)
  (if (find-a-ship object where)
      (find-cell (find-a-ship object where) where)
      (find-cell (sea object) where)))
