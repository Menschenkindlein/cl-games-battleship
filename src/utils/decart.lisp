(in-package :cl-games-battleship)

(defun convert-line-to-coords (n gsc)
  (if gsc
      (cons
       (floor (/ n (apply #'* (cdr gsc))))
       (convert-line-to-coords
	(- n (* (floor (/ n
			  (apply #'* (cdr gsc))))
		(apply #'* (cdr gsc))))
	(cdr gsc)))))

(defun convert-coords-to-line (coords gsc)
  (if gsc
      (+ (* (car coords)
	    (apply #'* (cdr gsc)))
	 (convert-coords-to-line (cdr coords) (cdr gsc)))
      0))

(defun lc-converter (sth gsc)
  (if (integerp sth)
      (convert-line-to-coords sth gsc)
      (convert-coords-to-line sth gsc)))

(defun cellp (object)
  (not (find-if-not #'integerp object)))

(defmacro def-cell-operation (name (constant cell-or-cells)
			      &body what-to-do-with-single-cell)
  `(defun ,name (,constant ,cell-or-cells)
     (cond
       ((cellp ,cell-or-cells)
	,@what-to-do-with-single-cell)
       ((listp (car ,cell-or-cells))
	(mapcar (lambda (cell)
		  (,name ,constant cell))
		,cell-or-cells))
       (t (error "Not cell or list of cells")))))

(def-cell-operation find-vector (from to)
  (mapcar #'- to from))

(def-cell-operation apply-vector (vector cell-or-cells)
  (mapcar #'+ vector cell-or-cells))

(def-cell-operation ort-reflect (reflector thing)
  (loop
     for ref in reflector
     for pt in thing
     collecting
       (if ref
	   (- ref (- pt ref))
	   pt)))

(defun appending-ort-reflect (reflector thing)
  (if (find-if-not #'listp thing)
      (error "Not list of cells")
      (remove-duplicates (append thing (ort-reflect reflector thing))
			 :test #'equal)))

(defun cube (lowest-cell dimensions)
  (apply-vector lowest-cell
		(loop for cell upto (- (apply #'* dimensions) 1)
		   collecting (lc-converter cell dimensions))))

(defun sphere-sector (dimensions-number radius &key border)
  (remove-if-not
   (lambda (cell)
     (funcall (if border #'= #'>)
	      radius (floor (apply #'square-root-of-squares-summ cell))))
   (cube (make-list dimensions-number :initial-element 0)
	 (loop repeat dimensions-number collecting (+ 1 radius)))))

(defun sphere (center radius &key (border t))
  (apply-vector center
		(let ((sphere (sphere-sector (length center) radius :border border))
		      (reflectors (loop for n upto (length center) collecting
				       (let ((reflector (make-list (length center) :initial-element 0)))
					 (if (nth n reflector) (setf (nth n reflector) nil))
					 reflector))))
		  (loop for reflector in reflectors
		     doing
		       (setf sphere (appending-ort-reflect reflector sphere)))
		  sphere)))

(defun aura (cells radius &key (border t))
  (let ((answer
	 (remove-duplicates
	  (apply #'append
		 (loop for cell in cells collecting
		      (sphere cell radius :border border)))))
	(deletion (if border
		      (apply #'append
			     (loop for cell in cells
				collecting
				  (sphere cell radius
					  :border nil))))))
    (if border
	(remove-if #'(lambda (cell)
		       (find cell deletion
			     :test #'equal))
		   answer)
	answer)))
