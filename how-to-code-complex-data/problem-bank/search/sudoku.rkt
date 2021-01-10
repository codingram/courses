#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) ; gets list-ref, take and drop

;;
;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;


;; =================
;; Data definitions:


;; Value is Natural[1, 9]

;; Board is (listof Value|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Position is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)


;; Convert 0-based row and column to Position
(define (row-col->pos row col) (+ (* row 9) col))  ; helpful for writing tests


;; Unit is (listof Position) of length 9
;; interp.
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

;; A sentinel used to denote that the specific spot is blank
(define B false)  ; B stands for blank


(define EMPTY-BOARD
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define ROW-BOARD
  (list 1 2 3 4 5 6 7 8 9
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define COLUMN-BOARD
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define SOLVABLE-BOARD1
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define SOLVABLE-BOARD1-SOLUTION
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define SOLVABLE-BOARD2
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B
        B B B B B 6 B B B
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define SOLVABLE-BOARD2-SOLUTION
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define SOLVABLE-BOARD3
  (list B B 5 3 B B B B B
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define SOLVABLE-BOARD3-SOLUTION
  (list 1 4 5 3 2 7 6 9 8
        8 3 9 6 5 4 1 2 7
        6 7 2 9 1 8 5 4 3
        4 9 6 1 8 5 3 7 2
        2 1 8 4 7 3 9 5 6
        7 5 3 2 9 6 4 8 1
        3 6 7 5 4 2 8 1 9
        9 8 4 7 6 1 2 3 5
        5 2 1 8 3 9 7 6 4))

(define SOLVABLE-BOARD4
  (list B B B 2 6 B 7 B 1
        6 8 B B 7 B B 9 B
        1 9 B B B 4 5 B B
        8 2 B 1 B B B 4 B
        B B 4 6 B 2 9 B B
        B 5 B B B 3 B 2 8
        B B 9 3 B B B 7 4
        B 4 B B 5 B B 3 6
        7 B 3 B 1 8 B B B))

(define NO-SOLUTION-BOARD
  (list 1 2 3 4 5 6 7 8 B
        B B B B B B B B 2
        B B B B B B B B 3
        B B B B B B B B 4
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))


;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))


;; =================
;; Functions:


;; --------------------------------------------------------------
;; Animation section

;; Constants:

(define BOX-WIDTH 90)
(define SQUARE-WIDTH (/ BOX-WIDTH 3))
(define TEXT-WIDTH (* SQUARE-WIDTH 0.8))
(define MOVIE-RATE 0.01)

;; Convenience constant so that I don't have to go at the bottom to change
;; which board to pass to the main function
(define CALL-MAIN-WITH SOLVABLE-BOARD2)

;; Functions:

;; Board -> (listof Image)
;; Runs the movie for the solution of the provided Board

(define (main board)
  (run-movie
   MOVIE-RATE
   (map board->image (list-solution board))))


;; Board -> Image
;; Renders the Board

(define (board->image board)
  (local
    [
     ; (listof (listof Position)) -> Image
     (define (boxes->image boxes)
       (letrec ([box-images (map box->image boxes)])
         (above
           (beside (first box-images) (second box-images) (third box-images))
           (beside (fourth box-images) (fifth box-images) (sixth box-images))
           (beside (seventh box-images) (eighth box-images) (ninth box-images)))))

     ; (listof Position) -> Image
     (define (box->image box)
       (letrec ([square-images (map square->image box)])
         (overlay
           (square BOX-WIDTH "outline" "black")
           (above
             (beside (first square-images) (second square-images) (third square-images))
             (beside (fourth square-images) (fifth square-images) (sixth square-images))
             (beside (seventh square-images) (eighth square-images) (ninth square-images))))))

     ; Position -> Image
     (define (square->image position)
       (letrec ([value (read-square board position)])
         (if (false? value)
           (square SQUARE-WIDTH "outline" "gray")
           (overlay/align
             "center" "center"
             (text (number->string value) TEXT-WIDTH "black")
             (square SQUARE-WIDTH "outline" "gray")))))]

    (boxes->image BOXES)))


;; Board -> (listof Board)
;; Produces a list of all the possible boards the algorithm went through
;; Used in animating the algorithm

(define (list-solution board)
  (local [(define (helper-board board)
            (if (solved? board)
                (list board)
                (cons board (helper-list-of-board (next-boards board)))))

          (define (helper-list-of-board lobd)
            (cond [(empty? lobd) (list false)]
                  [else
                   (local [(define try (helper-board (first lobd)))]
                     (if (not (false? (last try)))
                         try
                         (append (pop try)
                                 (helper-list-of-board (rest lobd)))))]))]

    (helper-board board)))


;; (listof X) -> (listof X)
;; Returns the given list with the last element removed

(define (pop lst)
  (cond [(empty? lst) empty]
        [else
          (take lst (- (length lst) 1))]))


;; ----------------------------------------------------------------
;; Board -> Board or false
;; Produce a solution for board; or false if board is unsolvable
;; Assume: board is valid

(define (solve board)
  (local [(define (helper-board board)
            (if (solved? board)
                board
                (helper-list-of-board (next-boards board))))

          (define (helper-list-of-board lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (helper-board (first lobd)))]
                     (if (not (false? try))
                         try
                         (helper-list-of-board (rest lobd))))]))]

    (helper-board board)))


;; Board -> Boolean
;; produce true if board is solved
;; Assume: board is valid, so it is solved if it is full

(define (solved? board)
  (andmap number? board))


;; Board -> (listof Board)
;; produce list of valid next boards from board
;; finds first empty square, fills it with Natural[1, 9], keeps only valid boards


(define (next-boards board)
  (filter valid-board? (fill-first-empty-square (first-empty-square board) board)))


;; Board -> Pos
;; Return the position of the first empty square
;; Assume the board constains atleast one empty square

(define (first-empty-square board)
  (cond [(empty? board) (error "The board is assumed to contain atleast one empty square")]
        [else
          (if (false? (first board))
            0
            (+ 1 (first-empty-square (rest board))))]))


;; Pos Board -> (listof Board)
;; Produces a list of board filled with 1 to 9 at the given position

(define (fill-first-empty-square position board)
  (local [(define (fill-one value)
            (fill-square board position (+ value 1)))]
    (build-list 9 fill-one)))


;; Board -> Boolean
;; Produces true if none of the unit contains any duplicate value, false otherwise

(define (valid-board? board)
  (local
    [(define (valid-units? list-of-unit)
       (andmap valid-unit? list-of-unit))
     (define (valid-unit? unit)
       (no-duplicates?
         (filter number?
                 (map read-position unit))))
     (define (read-position position)
       (read-square board position))
     (define (no-duplicates? list-of-value)
       (cond [(empty? list-of-value) true]
             [else
               (if (member (first list-of-value) (rest list-of-value))
                 false
                 (no-duplicates? (rest list-of-value)))]))]
    (valid-units? UNITS)))


;; Board Pos -> Value or false
;; Produce value at given position on board.

(define (read-square board position)
  (list-ref board position))


;; Board Pos Value -> Board
;; produce new board with val at given position

(define (fill-square board position new-value)
  (append (take board position)
          (list new-value)
          (drop board (add1 position))))


;; ============
;; Tests:


(check-expect (pop empty) empty)
(check-expect (pop '(1 2 3)) '(1 2))

(check-expect (read-square ROW-BOARD (row-col->pos 0 5)) 6)
(check-expect (read-square COLUMN-BOARD (row-col->pos 7 0)) 8)

(check-expect (fill-square EMPTY-BOARD (row-col->pos 0 0) 1)
              (cons 1 (rest EMPTY-BOARD)))

(check-expect (solve SOLVABLE-BOARD1) SOLVABLE-BOARD1-SOLUTION)
(check-expect (solve SOLVABLE-BOARD2) SOLVABLE-BOARD2-SOLUTION)
(check-expect (solve SOLVABLE-BOARD3) SOLVABLE-BOARD3-SOLUTION)
(check-expect (solve NO-SOLUTION-BOARD) false)

(check-expect (solved? SOLVABLE-BOARD1) false)
(check-expect (solved? SOLVABLE-BOARD1-SOLUTION) true)

(check-expect (next-boards (cons 1 (rest EMPTY-BOARD)))
              (letrec ([next (lambda (n) (cons 1 (cons (+ 2 n) (rest (rest EMPTY-BOARD)))))])
                (build-list 8 next)))

(check-expect (first-empty-square EMPTY-BOARD) 0)
(check-expect (first-empty-square (cons 2 (rest EMPTY-BOARD))) 1)
(check-expect (first-empty-square (cons 2 (cons 4 (rest (rest EMPTY-BOARD))))) 2)

(check-expect (fill-first-empty-square 0 EMPTY-BOARD)
              (list (cons 1 (rest EMPTY-BOARD))
                    (cons 2 (rest EMPTY-BOARD))
                    (cons 3 (rest EMPTY-BOARD))
                    (cons 4 (rest EMPTY-BOARD))
                    (cons 5 (rest EMPTY-BOARD))
                    (cons 6 (rest EMPTY-BOARD))
                    (cons 7 (rest EMPTY-BOARD))
                    (cons 8 (rest EMPTY-BOARD))
                    (cons 9 (rest EMPTY-BOARD))))

(check-expect (valid-board? EMPTY-BOARD) true)
(check-expect (valid-board? ROW-BOARD) true)
(check-expect (valid-board? COLUMN-BOARD) true)
(check-expect (valid-board? SOLVABLE-BOARD1) true)
(check-expect (valid-board? SOLVABLE-BOARD2) true)
(check-expect (valid-board? (cons 2 (rest ROW-BOARD))) false)
(check-expect (valid-board? (cons 2 (rest COLUMN-BOARD))) false)
(check-expect (valid-board? (fill-square SOLVABLE-BOARD1 1 6)) false)


;; Uncomment the below line to start the animation
;; Set the constant CALL-MAIN-WITH with the board constant to watch the animation for that board
(main CALL-MAIN-WITH)
