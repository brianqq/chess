(defpackage #:chess.moves
  (:use #:cl #:chess.bitboard))

(in-package #:chess.moves)
(deftype color () '(member :black :white))
;;; I'm gonna use the convention that 0 is bottom right square facing white
(defun pawn-advance (color pawns)
  (declare (type color color)
	   (type bitboard pawns))
  (ash
   pawns
   (* (if (eq color :white ) 1 -1)
      8)))

(defun pawn-attack-from-adv (color pawn-adv dir)
  (declare (type color color)
	   (type (member :east :west) dir)
	   (type bitboard pawn-adv))
  (let ((west-mask #.(make-bitboard #b01111111
				    #b01111111
				    #b01111111
				    #b01111111
				    #b01111111
				    #b01111111
				    #b01111111
				    #b01111111))
	(east-mask #.(make-bitboard #b11111110
				    #b11111110
				    #b11111110
				    #b11111110
				    #b11111110
				    #b11111110
				    #b11111110
				    #b11111110)))
    (ash (logand pawn-adv (if (eq dir :east) east-mask west-mask))
	 (if (eq dir :east) -1 1))))
