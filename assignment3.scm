;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; <Add your name and student number here>   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)
;; ;TESTS
;; ; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
;; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0)
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
        [(= axis 3)
            (if (> orientation 4)
                orientation
                (if(= orientation 1)
                    4
                    (- orientation 1)
                )
            )
        ]
		[(= axis 4)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 5) 1]
                    [(= orientation 6) 3]
                    [(= orientation 3) 5]
                    [(= orientation 1) 6]
                )
            )
        ]
    )
)
;; ;TESTS
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (makepair orient indx axes state) 
	;(print orient " " indx " " axes " " state)
	(if (pair? (car (index state indx))) (print "")
	(if(eqv? orient #t)
		(list (car (index state indx)) (recalculateOrientation (car (cdr (index state indx))) axes))
		(list (car (index state indx)) (car (cdr (index state indx))))
	))
)

(define (rotateX ispositive state)
	;(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "x")) ;;; *TODO* ;;;
	(if(eqv? ispositive #t)
		(list (list (makepair #t 4 0 state) (makepair #f 1 0 state) (makepair #t 0 0 state) (makepair #f 3 0 state) (makepair #t 6 0 state) (makepair #f 5 0 state) (makepair #t 2 0 state) (makepair #f 7 0 state)) (list "x"))
		(list (list (makepair #t 2 3 state) (makepair #f 1 3 state) (makepair #t 6 3 state) (makepair #f 3 3 state) (makepair #t 0 3  state) (makepair #f 5 3 state) (makepair #t 4 3 state) (makepair #f 7 3 state)) (list "X"))
	)
)


;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
   	;;(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "y")) ;;; *TODO* ;;;
	(if(eqv? ispositive #t)
		(list (list (index state 0) (index state 1) (index state 2) (index state 3) (makepair #t 5 1 state) (makepair #t 7 1 state) (makepair #t 4 1 state) (makepair #t 6 1 state)) (list "y"))
		(list (list (index state 0) (index state 1) (index state 2) (index state 3) (makepair #t 6 1 state) (makepair #t 4 1 state) (makepair #t 7 1 state) (makepair #t 5 1 state)) (list "Y"))
	)
)

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
	;;(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "z")) ;;; *TODO* ;;;
	(if(eqv? ispositive #t) 
		(list (list (makepair #t 1 2 state) (makepair #t 5 2 state) (index state 2) (index state 3) (makepair #t 0 2 state) (makepair #t 4 2 state) (index state 6) (index state 7)) (list "z"))
		(list (list (makepair #t 4 4 state) (makepair #t 0 4 state) (index state 2) (index state 3) (makepair #t 5 4 state) (makepair #t 1 4 state) (index state 6) (index state 7)) (list "Z")) 
	)
)

;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)
;; ;TESTS
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
;; ;------------------------------------------------------------

;:------------------TESTS by Banele-----------------
;;2> (print (equal? '((2 5) (1 1) (5 2) (4 1) (7 3) (6 4) (3 2) (8 3)) (rotateX #t '((5 1) (1 1) (3 1) (4 1) (2 5) (6 4) (7 2) (8 3)))) "\n")


;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
(define (generateSuccessorStates state prevMoves) 
    (list
	(list 
		(car (rotateX #t state))
		(car (rotateX #f state))
		(car (rotateY #t state))
		(car (rotateY #f state))
		(car (rotateZ #t state))
		(car (rotateZ #f state))
	)
	(list
		(append prevMoves (index (rotateX #t state) 1))
		(append prevMoves (index (rotateX #f state) 1))
		(append prevMoves (index (rotateY #t state) 1))
		(append prevMoves (index (rotateY #f state) 1))
		(append prevMoves (index (rotateZ #t state) 1))
		(append prevMoves (index (rotateZ #f state) 1))
	)    
        ;(list
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
         ;   '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ;)
        
        ;'(("x") ("X") ("y") ("Y") ("z") ("Z"))
    ) ;;; *TODO* ;;; 
)

;; ;TESTS
; (print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())(list(list (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))(car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))(car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))(car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))(car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))) (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))) )'(("x") ("X") ("y") ("Y") ("z") ("Z"))) )"\n")


;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth
(define (newGenStates n state prevMoves) 
	;(print n " " state " " prevMoves)
	(if (= n 0) 
		(list state prevMoves)
		(if (< (length prevMoves) n)
			(genhelper n state prevMoves) ;begin ;begin doesn't do results make a function instead
				;(append finalList (newGenStates n (index (rotateX #t state) 0)(append prevMoves (index (rotateX #t state) 1)) finalList))
				;(newline)
				;(append finalList (newGenStates n (index (rotateX #f state) 0)(append prevMoves (index (rotateX #f state) 1)) finalList))
				;(newline)
				;(append finalList (newGenStates n (index (rotateY #t state) 0)(append prevMoves (index (rotateY #t state) 1)) finalList))
				;(newline)
				;(append finalList (newGenStates n (index (rotateY #f state) 0)(append prevMoves (index (rotateY #f state) 1)) finalList))
				;(newline)
				;(append finalList (newGenStates n (index (rotateZ #t state) 0)(append prevMoves (index (rotateZ #t state) 1)) finalList))
				;(newline)
				;(append finalList (newGenStates n (index (rotateZ #f state) 0)(append prevMoves (index (rotateZ #f state) 1)) finalList))
		
			 ;)
			 (list state prevMoves)
		)
	)
)

(define finalList '())

(define (genhelper n state prevMoves) 
			
			(list 
				(list 
					(index (newGenStates n (index (rotateX #t state) 0)(append prevMoves (index (rotateX #t state) 1)))0)
					(index (newGenStates n (index (rotateX #f state) 0)(append prevMoves (index (rotateX #f state) 1)))0)
					(index (newGenStates n (index (rotateY #t state) 0)(append prevMoves (index (rotateY #t state) 1)))0)
					(index (newGenStates n (index (rotateY #f state) 0)(append prevMoves (index (rotateY #f state) 1)))0)
					(index (newGenStates n (index (rotateZ #t state) 0)(append prevMoves (index (rotateZ #t state) 1)))0)
					(index (newGenStates n (index (rotateZ #f state) 0)(append prevMoves (index (rotateZ #f state) 1)))0)
				)
				(list 
					(index (newGenStates n (index (rotateX #t state) 0)(append prevMoves (index (rotateX #t state) 1)))1)
					(index (newGenStates n (index (rotateX #f state) 0)(append prevMoves (index (rotateX #f state) 1)))1)
					(index (newGenStates n (index (rotateY #t state) 0)(append prevMoves (index (rotateY #t state) 1)))1)
					(index (newGenStates n (index (rotateY #f state) 0)(append prevMoves (index (rotateY #f state) 1)))1)
					(index (newGenStates n (index (rotateZ #t state) 0)(append prevMoves (index (rotateZ #t state) 1)))1)
					(index (newGenStates n (index (rotateZ #f state) 0)(append prevMoves (index (rotateZ #f state) 1)))1)
				)
			)
)				;list
				;(list 
				;	(index (newGenStates n (car (rotateX #t state)) (append prevMoves (index (rotateX #t state) 1))) 0)
				;	(index (newGenStates n (car (rotateX #f state)) (append prevMoves (index (rotateX #f state) 1))) 0)
				;	(index (newGenStates n (car (rotateY #t state)) (append prevMoves (index (rotateY #t state) 1))) 0)
				;	(index (newGenStates n (car (rotateY #f state)) (append prevMoves (index (rotateY #f state) 1))) 0)
				;	(index (newGenStates n (car (rotateZ #t state)) (append prevMoves (index (rotateZ #t state) 1))) 0)
				;	(index (newGenStates n (car (rotateZ #f state)) (append prevMoves (index (rotateZ #f state) 1))) 0)
				;)
				;(list
				;	(append prevMoves (index (newGenStates n (car (rotateX #t state)) (append prevMoves (index (rotateX #t state) 1))) 1))
				;	(append prevMoves (index (newGenStates n (car (rotateX #f state)) (append prevMoves (index (rotateX #f state) 1))) 1))
				;	(append prevMoves (index (newGenStates n (car (rotateY #t state)) (append prevMoves (index (rotateY #t state) 1))) 1))
				;	(append prevMoves (index (newGenStates n (car (rotateY #f state)) (append prevMoves (index (rotateY #f state) 1))) 1))
				;	(append prevMoves (index (newGenStates n (car (rotateZ #t state)) (append prevMoves (index (rotateZ #t state) 1))) 1))
				;	(append prevMoves (index (newGenStates n (car (rotateZ #f state)) (append prevMoves (index (rotateZ #f state) 1))) 1))
				;)    
;			)
;		)
;	)
;)

(define (genStates n state moves)
	(define biglist (generateSuccessorStates state moves))
	(print n " " state " " moves "\n")
	(if(= 0 n)
		state
		;(if (= n 1)
			;biglist
			(begin ;genhelp (generateSuccessorStates state moves) n)
				(genStates (- n 1) (index (index biglist 0) 0) (index (index biglist 1) 0))
				(newline)
				(genStates (- n 1) (index (index biglist 0) 1) (index (index biglist 1) 1))
				(newline)
				(genStates (- n 1) (index (index biglist 0) 2) (index (index biglist 1) 2))
				(newline)
				(genStates (- n 1) (index (index biglist 0) 3) (index (index biglist 1) 3))
				(newline)
				(genStates (- n 1) (index (index biglist 0) 4) (index (index biglist 1) 4))
				(newline)
				(genStates (- n 1) (index (index biglist 0) 5) (index (index biglist 1) 5))
		;	)
		)
	)
    ;;((((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) ((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3)) ((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3)) ((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3)) ((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3)) ((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3))) ((x) (X) (y) (Y) (z) (Z))) ;;; *TODO* ;;;
)

(define statelist (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()))
(define (genhelp biglist n) 
	(genStates (- n 1) (index (index biglist 0) 0) (index (index biglist 1) 0))
	(genStates (- n 1) (index (index biglist 0) 1) (index (index biglist 1) 1))
	(genStates (- n 1) (index (index biglist 0) 2) (index (index biglist 1) 2))
	(genStates (- n 1) (index (index biglist 0) 3) (index (index biglist 1) 3))
	(genStates (- n 1) (index (index biglist 0) 4) (index (index biglist 1) 4))
	(genStates (- n 1) (index (index biglist 0) 5) (index (index biglist 1) 5))
)
;----------------------------------------------------------


;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
(define (solveCube solved initial n)
    '("Z", "Y", "X") ;;; *TODO* ;;;
)
;---------------------------------------------------------------------
;TESTS
; (print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
; (print (equal? '("X") (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;---------------------------------------------------------------------

;-------------rotateX tests----------------------
;(rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))
;EXEPECTED ((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3))
;> (rotateX #t '((5 1) (1 1) (3 1) (4 1) (2 5) (6 4) (7 2) (8 3)))
;((2 5) (1 1) (5 2) (4 1) (7 3) (6 4) (3 2) (8 3))


