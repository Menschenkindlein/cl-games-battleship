(defpackage :cl-games-battleship (:use :cl)
	    (:export :game-space
		     :cleared
		     :shoot
		     :killer
		     :ask
		     :change-killing-sequence
		     :constant-placer
		     :random-placer-bf
		     :cube
		     :aura
		     :sphere
		     :perforated-sphere
		     :print-game-space
		     :start-game-human
		     :start-easy-game))

(in-package :cl-games-battleship)