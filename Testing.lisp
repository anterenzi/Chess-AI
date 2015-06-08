;;; =========================
;;;  CMPU-365, Fall 2014
;;;  Asmt. 5
;;;  a5-test.lisp
;;; =========================
;;;  Some functions for testing the chess implementation in 
;;;  a5.lisp and a5-plus.lisp

;;;  For convenience... 

;;;  BUILD
;;; -------------------------------------
;;;  INPUT:  YOUR-FILE-NAME, a STRING that holds the name of your Lisp
;;;            file, but without any extension (e.g., "a5-plus").
;;;  OUTPUT:  NIL
;;;  SIDE-EFFECT:  Compiles-and-loads the files necessary to test your
;;;                chess functions.

(defun build (your-file-name)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  
  (compile-file "a5-at")
  (load "a5-at")
  (compile-file your-file-name)
  (load your-file-name)
  nil
  )

;;;  TEST-CHESS
;;; ---------------------------------------
;;;  INPUT:  LIST-O-MOVES, a list of "moves" where each move is 
;;;             a list of 4 numbers (of the kind used by do-move!)
;;;  OUTPUT:  A recommended move for the situation that arises from
;;;           doing the moves in the input list in a new game of chess.
;;;  SIDE-EFFECT:  Changes the values of the global variables,
;;;                *CUTOFF-DEPTH* and G.

(defun test-chess (list-o-moves)
  ;; Start a new game
  (setf g (init-game))
  ;; For each move in the LIST-O-MOVES...
  (mapcar #'(lambda (movie)
	      ;; Do the move...
	      (apply #'do-move! g nil movie))
	  list-o-moves)
  ;; Show the resulting state of the game...
  (print-chess g t nil)
  ;; Set the cutoff-depth to be used by COMPUTE-MOVE
  (setf *cutoff-depth* 6)
  ;; Compute a recommended move!!
  (compute-move g))

;;;  TEST-ONE -- White should find the easy checkmate!

(defun test-one ()
  (test-chess '((1 4 3 4)
		(6 0 4 0)
		(0 3 2 5)
		(7 0 5 0)
		(0 5 3 2)
		(6 7 4 7)
		;; It will be white's turn when COMPUTE-MOVE is called
		)))

;;;  TEST-TWO -- White should be able to win the rook in two moves!

(defun test-two ()
  (test-chess '((1 4 3 4)
		(6 4 4 4)
		(0 6 2 5)
		(6 3 5 3)
		(0 5 4 1)
		(7 4 6 4)
		(2 5 4 6)
		(7 1 5 0)
		(4 1 3 2)
		(6 7 5 7)
		;; It will be white's turn when COMPUTE-MOVE is called
		)))

;;;  TEST-THREE -- Black should move the queen to avoid the pin!

(defun test-three ()
  (test-chess '((1 4 3 4)
		(6 4 4 4)
		(0 6 2 5)
		(6 3 5 3)
		(0 5 4 1)
		(7 4 6 4)
		(2 5 4 6)
		(7 1 5 0)
		(4 1 3 2)
		(6 7 5 7)
		(4 6 6 5)
		;; It will be black's turn when COMPUTE-MOVE is called
		)))
