(in-package :cl-games-battleship)

(defun-sphere-and-aura 2)

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
		   (collect-the-lowest-level
		    (make-instance 'sea-cell :coords (list x y))
		    (x from -1 upto (first gsconfig))
		    (y from -1 upto (second gsconfig))))))

(defclass ship (multy-cell-object)
  ((coordinates :initarg :coords
		:reader coords)
   (size :initarg :size
	   :reader size)
   (direction-verticalp :initarg :direction
			:reader direction)
   (alive :initform t
	  :accessor alive)))

(defclass real-ship (ship) ;; The difference is neaded for killer AI
   ((aura :accessor aura)))

(defmethod initialize-instance :after ((ship ship) &key)
  (with-accessors ((coords coords)
		   (size size)
		   (direction direction)) ship
    (setf (own-cells ship)
	  (loop for i upto size
	     collecting
	       (make-instance 'ship-cell
			      :coords
			      (list
			       (if direction
				   (+ (first coords) i)
				   (first coords))
			       (if direction
				   (second coords)
				   (+ (second coords) i))))))))

(defmethod initialize-instance :after ((ship real-ship) &key)
  (let ((own-cells (loop for cell in (own-cells ship)
			collecting (coords cell))))
    (setf (aura ship)
	  (remove-if
	   #'(lambda (x) 
	       (find-cell ship x))
	   (aura-2d own-cells 1)))))

(defgeneric shoot (object where))

(defmethod shoot ((object sea) where)
  (shoot-cell (find-cell object where))
  :missed)

(defgeneric shoot-ship (ship sea where))

(defmethod shoot-ship ((ship real-ship) (sea sea) where)
  (shoot-cell (find-cell ship where))
  (if (not (find-if-not #'(lambda (cell)
			    (shooted cell))
			(own-cells ship)))
      (progn
	(setf (alive ship) nil)
	(loop for cell in (aura ship) doing
	     (shoot sea cell))
	:killed)
      (progn
	(loop for cell in
	     (remove-if #'(lambda (x)
			    (or (= (first x)
				   (first where))
				(= (second x)
				   (second where))))
			(aura-2d (list where) 1))
	   doing
	     (shoot sea cell))
	:shooted)))

(defclass game-space ()
  ((game-space-config :initarg :gsconfig
		      :reader gsconfig)
   (ships-config :initarg :shconfig
		 :reader shconfig)
   (ships :accessor ships)
   (sea :accessor sea)
   (correct :accessor correct)))

(defun check-for-collapsing-ships (ships gsconfig)
  (let ((all-own-cells (mapcar #'(lambda (cell)
				   (coords cell))
			       (apply #'append
				      (collect-the-lowest-level
				       (own-cells ship)
				       (ship in ships)))))
	(all-nearest-cells (apply #'append
				  (collect-the-lowest-level
				   (aura ship)
				   (ship in ships)))))
    (find-if #'(lambda (cell)
		 (let ((x (first cell))
		       (y (second cell)))
		   (or (> 0 x)
		       (< (- (first gsconfig) 1) x)
		       (> 0 y)
		       (< (- (second gsconfig) 1) y)
		       (find cell all-nearest-cells :test #'equal)
		       (< 1 (count cell all-own-cells :test #'equal)))))
	     all-own-cells)))

(defmethod initialize-instance :after ((game-space game-space)
				       &key ships-positions)
  (setf (ships game-space)
	(loop for ship in ships-positions
	   collecting
	     (make-instance 'real-ship
			    ;; Converting numbers into indexes
			    :size (- (first ship) 1)
			    :coords (list (- (first (second ship)) 1)
					  (- (second (second ship)) 1))
			    :direction (third ship))))
  (if (check-for-collapsing-ships (ships game-space) (gsconfig game-space))
      (setf (correct game-space) nil)
      (setf (correct game-space) t))
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
  ;; Make numbers indexes
  (let ((where (list (- (first where) 1)
		     (- (second where) 1))))
    (if (find-a-ship game-space where)
	(shoot-ship (find-a-ship game-space where) (sea game-space) where)
	(shoot (sea game-space) where))))

(defmethod find-cell ((object game-space) where)
  (if (find-a-ship object where)
      (find-cell (find-a-ship object where) where)
      (find-cell (sea object) where)))

