(in-package :cl-games-battleship)

;; Works only on greater than 10x10 and (4 3 3 2 2 2 1 1 1 1) ships-config
(defun constant-placer (game-space-config ships-config)
  (if (and (= 2 (length game-space-config))
	   (< 9 (first game-space-config))
	   (< 9 (second game-space-config))
	   (equal '(4 3 3 2 2 2 1 1 1 1) ships-config))
      '((4 (1 1) 2)
	(3 (3 1) 2)
	(3 (5 1) 2)
	(2 (7 1) 2)
	(2 (9 1) 2)
	(2 (1 9) 2)
	(1 (3 10) 2)
	(1 (5 10) 2)
	(1 (7 10) 2)
	(1 (9 10) 2))
      (error "I can not work with this config!")))

(defun random-placer-bf (game-space-config ships-config)
  (format t "I'm placing my ships...") (finish-output)
  (loop for try from 1 by 1 doing
       (let ((places (loop for ship in ships-config collecting
			  (list ship
				(loop for coord in game-space-config
				   collecting
				     (+ 1 (random coord)))
				(+ 1 (random (length game-space-config)))))))
	 (when (= 0 (rem try 1000)) (princ ".") (finish-output))
	 (when (= 100000 try) (error "This config is too difficult for me."))
	 (when (correct (make-instance 'game-space
				       :gsconfig game-space-config
				       :ships-positions
				       places))
	   (format t "done~%Uf!  I've tried ~:d time~:p to place my ships!~%" try)
	   (finish-output)
	   (return places)))))