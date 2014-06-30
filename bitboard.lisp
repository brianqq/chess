(defpackage #:chess.bitboard
  (:use #:cl)
  (:export :bitboard
	   :bitscan
	   :popcount
	   :make-bitboard))

(in-package #:chess.bitboard)

;;; from http://recurial.com/programming/efficient-bit-scaning-in-lisp/
;;; I don't need debruijn stuff if I am using vops 
;;; (defconstant +debruijn64+ #x07EDD5E59A4E28C2)

;; (defun bitscan (bb)
;;   (declare (optimize (speed 3))
;; 	   (type (unsigned-byte 64) bb))
;;   (aref
;;    #(63 0 58 1 59 47 53 2
;;      60 39 48 27 54 33 42 3
;;      61 51 37 40 49 18 28 20
;;      55 30 34 11 43 14 22 4
;;      62 57 46 52 38 26 32 41
;;      50 36 17 19 29 10 13 21
;;      56 45 25 31 35 16 9 12
;;      44 24 15  8 23 7 6 5)
;;    (ash (ldb (byte 64 0)
;; 	     (* (logand bb (- bb)) +debruijn64+))
;; 	-58)))

(sb-c:defknown %bsf ((unsigned-byte 64))
    (unsigned-byte 32)
    (sb-c::movable sb-c::foldable sb-c::flushable))
(sb-c:define-vop (%bsf)
  (:policy :fast-safe) 			;this might be a blatant lie
  (:translate %bsf)
  (:note "Scans forward for the first 1 bit")
  (:args (a :scs (sb-vm::unsigned-reg) :target b))
  (:arg-types sb-vm::unsigned-byte-64)
  (:results (b :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 0 (sb-c::inst sb-vm::bsf b a)))
(defun %bsf (a)
  (%bsf a))

(sb-c:defknown %bsr ((unsigned-byte 64))
    (unsigned-byte 32)
    (sb-c::movable sb-c::foldable sb-c::flushable))
(sb-c:define-vop (%bsr)
  (:policy :fast-safe) 			;this might be a blatant lie
  (:translate %bsr)
  (:note "Scans forward for the first 1 bit")
  (:args (a :scs (sb-vm::unsigned-reg) :target b))
  (:arg-types sb-vm::unsigned-byte-64)
  (:results (b :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 0 (sb-c::inst sb-vm::bsr b a)))
(defun %bsr (a)
  (%bsr a))


(declaim (inline bitscan))
(defun bitscan (bb)
  (declare (type (unsigned-byte 64) bb))
  (%bsf bb))
;;; ------------------------------------------------------------

(deftype bitboard () '(unsigned-byte 64))

(defvar *pop-count-byte* (make-array 256
				     :element-type '(unsigned-byte 64)
				     :initial-element 0
				     :adjustable nil))
(loop for i from 1 to 255 do
     (setf (aref *pop-count-byte* i)
	   (+ (aref *pop-count-byte* (truncate i 2))
	      (logand i 1))))

(defun pop-count (board)
  (declare (type bitboard board))
  (loop for pos from 0 to (* 7 8) by 8
     summing (aref *pop-count-byte* (ldb (byte 8 pos) board))))

;;; http://chessprogramming.wikispaces.com/General+Setwise+Operations#ShiftingBitboards

;;; http://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating

(defun make-bitboard (&rest bits)
  (loop for x in (reverse bits)
     for i from 0 by 8
     summing (ash x i)))
