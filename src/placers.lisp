(in-package :cl-games-battleship)

;; Works only on greater than 10x10 and (4 3 3 2 2 2 1 1 1 1) ships-config
(defun constant-placer (game-space-config ships-config)
  (if (and (= 2 (length game-space-config))
	   (< 9 (first game-space-config))
	   (< 9 (second game-space-config))
	   (equal '(4 3 3 2 2 2 1 1 1 1) ships-config))
      '((4 (1 1) nil)
	(3 (3 1) nil)
	(3 (5 1) nil)
	(2 (7 1) nil)
	(2 (9 1) nil)
	(2 (1 9) nil)
	(1 (3 10) nil)
	(1 (5 10) nil)
	(1 (7 10) nil)
	(1 (9 10) nil))
      (error "I can not work with this config!")))

(defun random-placer-bf (game-space-config ships-config)
  (loop for try from 1 by 1 doing
       (let ((places (loop for ship in ships-config collecting
			  (list ship
				(loop for coord in game-space-config
				     collecting
				     (+ 1 (random coord)))
				(if (= 1 (random (length game-space-config)))
				    t)))))
	 (when (correct (make-instance 'game-space
				       :shconfig ships-config
				       :gsconfig game-space-config
				       :ships-positions
				       places))
	   (format t "Uf!  I've tried ~:d times to place my ships!~%" try)
	   (return places)))))
