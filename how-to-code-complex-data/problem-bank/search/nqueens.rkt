#lang htdp/asl

(require racket/list)

;; This project involves the design of a program to solve the n queens puzzle.
;;
;; This starter file explains the problem and provides a few hints you can use
;; to help with the solution.
;;
;; The key to solving this problem is to follow the recipes! It is a challenging
;; problem, but if you understand how the recipes lead to the design of a Sudoku
;; solve then you can follow the recipes to get to the design for this program.
;;
;;
;; The n queens problem consists of finding a way to place n chess queens
;; on a n by n chess board while making sure that none of the queens attack each
;; other.
;;
;; The BOARD consists of n^2 individual SQUARES arranged in 4 rows of 4 columns.
;; The colour of the squares does not matter. Each square can either be empty
;; or can contain a queen.
;;
;; A POSITION on the board refers to a specific square.
;;
;; A queen ATTACKS every square in its row, its column, and both of its diagonals.
;;
;; A board is VALID if none of the queens placed on it attack each other.
;;
;; A valid board is SOLVED if it contains n queens.
;;
;;
;; There are many strategies for solving nqueens, but you should use the following:
;;
;;   - Use a backtracking search over a generated arb-arity tree that
;;     is trying to add 1 queen at a time to the board. If you find a
;;     valid board with 4 queens produce that result.
;;
;;   - You should design a function that consumes a natural - N - and
;;     tries to find a solution.
;;
;;
;;
;; NOTE 1: You can tell whether two queens are on the same diagonal by comparing
;; the slope of the line between them. If one queen is at row and column (r1, c1)
;; and another queen is at row and column (r2, c2) then the slope of the line
;; between them is: (/ (- r2 r1) (- c2 c1)).  If that slope is 1 or -1 then the
;; queens are on the same diagonal.


;; ------------------
;; Data definitions:

;; Index is Natural[0, (sqr n))
;; interp. location of the cell on the board in the form of zero-based index where
;;         0 <= index <= (sub1 (sqr n)), n being the length of the board

;; Board is (listof Index)
;; interp. represents the board as a single list of cell where
;;         (length Board) is (sqr n), n being the length of the board


;; -------------------
;; Functions:

;; -----------------------------------------
;; Generate all steps to reach the solution
;; -----------------------------------------

;; Natural -> (listof Board) or false
;; List all the steps taken to find the final solution

(define (list-solution n)
  (local
    [
     ; Board -> (listof Board) or false
     (define (helper-board board)
       (if (= (length board) n)
         (list board)
         (cons board (helper-list-of-board (next-boards board n)))))

     ; (listof Board) -> (listof Board) or false
     (define (helper-list-of-board list-of-board)
       (if (empty? list-of-board)
         (list false)
         (let ([try (helper-board (first list-of-board))])
           (if (not (false? (last try)))
             try
             (append (pop try)
                     (helper-list-of-board (rest list-of-board)))))))]

    (helper-board empty)))


;; (listof X) -> (listof X)
;; Returns the given list with the last element removed

(define (pop lst)
  (if (empty? lst)
    empty
    (take lst (sub1 (length lst)))))

;; ------------------------------------------

;; Natural -> (listof Board) or false
;; For the given input n, return the list of board to the solution board if the solution exists
;; for the nqueen problem, false if there's no solution for the given n.

(define (solve n)
  (local
    [
     ; Board -> (listof Board) or false
     (define (helper-board board)
       (if (= (length board) n)
         (list board)
         (let ([result (helper-list-of-board (next-boards board n))])
           (if (not (false? result))
             (cons board result)
             false))))

     ; (listof Board) -> (listof Board) or false
     (define (helper-list-of-board list-of-board)
       (if (empty? list-of-board)
         false
         (let ([try (helper-board (first list-of-board))])
           (if (not (false? try))
             try
             (helper-list-of-board (rest list-of-board))))))]

    (helper-board empty)))


;; Board -> (listof Board)
;; Produces the next valid board from the given (current) board.

(define (next-boards board n)
  (map (lambda (valid-position) (cons valid-position board))
       (filter (lambda (next-position)
                 (andmap (lambda (current-position)
                           (not (attack? next-position current-position n)))
                         board))
               (build-list (sqr n) identity))))


;; Index Index -> Boolean
;; Determine whether the given two position of the queen attack one another.

(define (attack? next-position current-position n)
  (let ([next-pos-x (remainder next-position n)]
        [next-pos-y (quotient next-position n)]
        [curr-pos-x (remainder current-position n)]
        [curr-pos-y (quotient current-position n)])
    (or (= next-pos-x curr-pos-x)
        (= next-pos-y curr-pos-y)
        (= (abs (/ (- curr-pos-y next-pos-y)
                   (- curr-pos-x next-pos-x))) 1))))


;; -------------------
;; Tests:


(check-expect (solve 3) #f)
(check-expect (last (solve 4)) '(14 8 7 1))
(check-expect (last (solve 5)) '(23 16 14 7 0))

(check-expect (next-boards empty 4) (build-list 16 (lambda (n) `(,n))))

(check-expect (attack? 2 0 4) true)
(check-expect (attack? 12 4 4) true)
(check-expect (attack? 11 1 4) true)
(check-expect (attack? 5 15 4) true)
(check-expect (attack? 0 9 4) false)
(check-expect (attack? 8 6 4) false)
