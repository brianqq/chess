(asdf:defsystem #:chess
  :serial t
  :author "Brian Levy <brian_levy@brown.edu>"
  :depends-on (#:alexandria)
  :components ((:file "bitboard")
	       (:file "moves")))
