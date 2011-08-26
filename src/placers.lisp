(in-package :cl-games-battleship)

;; Works only on greater than 10x10 and (4 3 3 2 2 2 1 1 1 1) ships-config
(defun constant-placer (game-space-config ships-config)
  (if (and (< 9 (first game-space-config))
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
  (loop (let ((places (loop for ship in ships-config collecting
			   (list ship
				 (list (+ 1 (random
					     (first game-space-config)))
				       (+ 1 (random
					     (second game-space-config))))
				 (if (= 1 (random 2))
				     t)))))
	  (if (correct (make-instance 'game-space
				      :shconfig ships-config
				      :gsconfig game-space-config
				      :ships-positions
				      places))
	      (return places)))))