(in-package :cl-games-battleship)

(defmacro collect-the-lowest-level (form &rest levels)
  `(apply #'append
	  (loop for ,(car (car levels)) ,@(cdr (car levels))
	     collecting
	       ,(if (= 1 (length levels))
		    `(list ,form)
		    `(collect-the-lowest-level ,form ,@(cdr levels))))))

(defclass cell ()
  ((x-coordinate :initarg :x
		 :reader x)
   (y-coordinate :initarg :y
		 :reader y)
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
		  (and (= (first where) (x cell))
		       (= (second where) (y cell))))))

(defclass sea (multy-cell-object) ())

(defmethod initialize-instance :after ((sea sea) &key gsconfig ships)
  (setf (own-cells sea)
	(remove-if #'(lambda (cell)
		       (find-if #'(lambda (ship)
				    (find-cell ship (list (x cell) (y cell))))
				ships))
		   (collect-the-lowest-level
		    (make-instance 'sea-cell :x x :y y)
		    (x upto (first gsconfig))
		    (y upto (second gsconfig))))))

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
   ((nearest-cells :accessor nearest-cells)))

(defmethod initialize-instance :after ((ship ship) &key)
  (with-accessors ((coords coords)
		   (size size)
		   (direction direction)) ship
    (setf (own-cells ship)
	  (loop for i upto size
	     collecting
	       (make-instance 'ship-cell
			      :x (if direction
				     (+ (first coords) i)
				     (first coords))
			      :y (if direction
				     (second coords)
				     (+ (second coords) i)))))))

(defmethod initialize-instance :after ((ship real-ship) &key)
  (setf (nearest-cells ship)
	(remove-duplicates
	 (remove-if
	  #'(lambda (x) 
	      (or
	       (find-cell ship x)
	       (> 0 (first x))
	       (> 0 (second x))))
	  (collect-the-lowest-level
	   (list (+ (x cell) i)
		 (+ (y cell) j))
	   (cell in (own-cells ship))
	   (i from -1 to 1)
	   (j from -1 to 1))))))

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
	(loop for cell in (nearest-cells ship) doing
	     (shoot sea cell))
	:killed)
      (progn
	(loop for cell in (remove-if #'(lambda (x)
					 (or (= (first x)
						(first where))
					     (= (second x)
						(second where))
					     (< 1 (expt
						   (- (first x)
						      (first where)) 2))
					     (< 1 (expt
						   (- (second x)
						      (second where)) 2))))
				     (nearest-cells ship))
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
  (let ((all-own-cells (apply #'append
			      (collect-the-lowest-level
			       (own-cells ship)
			       (ship in ships))))
	(all-nearest-cells (apply #'append
				  (collect-the-lowest-level
				   (nearest-cells ship)
				   (ship in ships)))))
    (find-if #'(lambda (cell)
		 (let ((x (first cell))
		       (y (second cell)))
		   (or (> 0 x)
		       (< (first gsconfig) x)
		       (> 0 y)
		       (< (second gsconfig) y)
		       (find (list x y) all-nearest-cells)
		       (< 1 (count cell all-own-cells)))))
	     all-nearest-cells)))

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