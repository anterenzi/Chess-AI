;;; ============================
;;;  CMPU-365, Fall 2014
;;;  a5-plus.lisp
;;;  ANDREW TERENZI
;;; ============================

;;;  COMPUTE-MOVE
;;; -------------------------------------------------------------
;;;  INPUT:  G, a CHESS struct
;;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;;   a depth of *CUTOFF-DEPTH*.

(defun compute-move (g)
  
  ;; Reset the global variable for node count
  (setf *node-count* 0)
  
  ;; Start the problem!
  (let* ((best-move (compute-max g 0 *alpha-max* *beta-max* nil)))
    (format t "The amount of moves computed was: ~A~%" *node-count*)
    
    ;;;"Best-move" is going to be the next best move found
    best-move))
    

(defparameter *cutoff-depth* 5)

;;;Initial values for alpha and beta!
(defparameter *alpha-max* -10000000)
(defparameter *beta-max* 10000000)
(defparameter *node-count* 0)

;;; COMPUTE-MAX
;;; ------------------------------
;;; INPUT: G, a CHESS struct
;;; N, CURRENT LEVEL
;;; A, alpha value
;;; B, beta value
;;; M, the 'best move so far'
;;; OUTPUT: ALPHA VALUE
;;; OR best move so far (at depth 0)

(defun compute-max (g n alpha beta m)
  (cond
   
   ;;CASE 1: Did the move end the game?
   ((game-over? g)
    -100000)
   
   ;;CASE 2: Was the cutoff depth reached?
   ;; Then use the static evaluation function!
   ((equal n *cutoff-depth*)
    (eval-func g))
   
   ;; CASE 3: Otherwise...
   (t
    ;;; For EACH legal moves..
    (mapcar #'(lambda (x)
		
		;; Do the move
		(apply #'do-move! g nil x)
		(incf *node-count*)
		
		;; Then get the beta value for the node
		;; at the next depth!!
		(let* ((beta-value (compute-min
				    g (+ 1 n) alpha beta m)))
		  
		  ;; Undo the move
		  (undo-move! g)
		  
		  ;; Check if alpha needs to be updated!
		  (cond
		   ((> beta-value alpha)
		    (setf alpha beta-value)
		    
		    ;; Also, keep track of that move for later
		    (setf m x)
		    
		    ;; Is there a pruning opportunity?
		    (if (<= beta alpha)
			(return-from compute-max alpha))
		    ))))
	    (legal-moves g))
    
    ;; If the depth is not 0, return alpha. Otherwise the best move!
    (if (equal n 0)
	m
      alpha))))

;;; COMPUTE-MIN
;;; ------------------------------
;;; INPUT: G, a CHESS struct
;;; N, CURRENT LEVEL
;;; B, beta value
;;; M, the 'best move so far'
;;; OUTPUT: BETA VALUE

(defun compute-min (g n alpha beta m)
  (cond
   
   ;; CASE 1: Did the move end the game?
   ((game-over? g)
    100000)
   
   ;; CASE 2: Was the cutoff depth reached?
   ;; Then use the static evaluation function!
   ((equal n *cutoff-depth*)
    (eval-func g))
   
   ;; CASE 3: Otherwise...
   (t
    ;; For EACH legal move...
    (mapcar #'(lambda (x)
		
		;; Do the move
		(apply #'do-move! g nil x)
		(incf *node-count*)
		
		;; Then get the alpha value for the node
		;; at the next depth!!
		(let* ((alpha-value (compute-max
				     g (+ 1 n) alpha beta m)))
		  
		  ;; Undo the move
		  (undo-move! g)
		  
		  ;; Check if beta needs to be updated
		  (cond
		   ((< alpha-value beta)
		    (setf beta alpha-value)
		    
		    ;; Keep track of the move for later
		    (setf m x)
		    
		    ;; Is there a pruning opportunity?
		    (if (<= beta alpha)
			(return-from compute-min beta))))))
	    (legal-moves g))
    
    ;; Return the final beta value!
    beta)))

;;; EVAL-FUNC
;;; ------------------
;;; INPUT: G, a CHESS struct
;;; N, current depth (for turn)
;;; OUTPUT: A value that repesents the
;;; static evaluation function

(defun eval-func (g)
  
  ;; Get the two subtotals from eval-subtotals
  (let* ((white-subtotal (svref (chess-eval-subtotals g) 0))
	 (black-subtotal (svref (chess-eval-subtotals g) 1)))
    
    ;; The turn matters!!! Find the difference
    (if (equal (chess-whose-turn? g) 0)
	(- white-subtotal black-subtotal)
      (- black-subtotal white-subtotal))))


;;; This is the ghetto easy version
;;; I used this just to test
(defun eval-func2 (g)
  (let* ((all-pieces (chess-pieces g))
	 (total 0))
    (dotimes (i 2)
      (dotimes (j 16)
	(let* ((current-piece (aref all-pieces i j))
	       (type (piece-type current-piece))
	       (current-owner (piece-owner current-piece)))
	  (cond
	   ((not (piece-live? current-piece))
	    0)
	   ((and
	     (eq type *pawn*)
	     (equal current-owner 0))
	    (setf total (+ total 100)))
	   ((and
	     (eq type *knight*)
	     (equal current-owner 0))
	    (setf total (+ total 300)))
	   ((and
	     (eq type *bishop*)
	     (equal current-owner 0))
	    (setf total (+ total 350)))
	   ((and
	     (eq type *rook*)
	     (equal current-owner 0))
	    (setf total (+ total 500)))
	   ((and
	     (eq type *queen*)
	     (equal current-owner 0))
	    (setf total (+ total 975)))
	   ((and
	     (eq type *king*)
	     (equal current-owner 0))
	    (setf total (+ total 100000)))
	   ((and
	     (eq type *pawn*)
	     (equal current-owner 1))
	    (setf total (- total 100)))
	   ((and
	     (eq type *knight*)
	     (equal current-owner 1))
	    (setf total (- total 300)))
	   ((and
	     (eq type *bishop*)
	     (equal current-owner 1))
	    (setf total (- total 350)))
	   ((and
	     (eq type *rook*)
	     (equal current-owner 1))
	    (setf total (- total 500)))
	   ((and
	     (eq type *queen*)
	     (equal current-owner 1))
	    (setf total (- total 975)))
	   ((and
	     (eq type *king*)
	     (equal current-owner 1))
	    (setf total (- total 100000)))))))
    total))