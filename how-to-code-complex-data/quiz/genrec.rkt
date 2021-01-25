#lang htdp/asl

(require 2htdp/image)

;; PROBLEM 1:
;;
;; In the lecture videos we designed a function to make a Sierpinski triangle fractal.
;;
;; Here is another geometric fractal that is made of circles rather than triangles:
;;
;; Design a function to create this circle fractal of size n and colour c.


(define CUT-OFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(define (circle-fractal size color)
  (if (<= size CUT-OFF)
    (circle size 'outline color)
    (overlay
      (circle size 'outline color)
      (let ([sub (circle-fractal (/ size 2) color)])
        (beside sub sub)))))


(define C0 (circle CUT-OFF "outline" "red"))
(define C1 (circle (* 2 CUT-OFF) "outline" "red"))
(define C2 (circle (* 4 CUT-OFF) "outline" "red"))

(check-expect (circle-fractal CUT-OFF "red") C0)
(check-expect (circle-fractal (* 2 CUT-OFF) "red")
              (overlay C1 (beside C0 C0)))
(check-expect (circle-fractal (* 4 CUT-OFF) "red")
              (overlay C2
                       (beside (overlay C1 (beside C0 C0))
                               (overlay C1 (beside C0 C0)))))




;; PROBLEM 2:
;;
;; Below you will find some data definitions for a tic-tac-toe solver.
;;
;; In this problem we want you to design a function that produces all
;; possible filled boards that are reachable from the current board.
;;
;; In actual tic-tac-toe, O and X alternate playing. For this problem
;; you can disregard that. You can also assume that the players keep
;; placing Xs and Os after someone has won. This means that boards that
;; are completely filled with X, for example, are valid.
;;
;; Note: As we are looking for all possible boards, rather than a winning
;; board, your function will look slightly different than the solve function
;; you saw for Sudoku in the videos, or the one for tic-tac-toe in the
;; lecture questions.


;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X"))

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))

(define B4 (list "X" "O" "X"       ; filled board
                 "O" "X" "O"
                 "O" "O" "X"))

(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))


;; Board -> (listof Board)
;; Produce a list of all possible boards from the given board

(define (all-boards board)
  (cond [(empty? board) (list empty)]
        [else
          (let ([try (all-boards (rest board))])
            (if (false? (first board))
              (append
                (map (lambda (try-board) (cons "X" try-board)) try)
                (map (lambda (try-board) (cons "O" try-board)) try))
              (map (lambda (try-board) (cons (first board) try-board)) try)))]))



(check-expect (all-boards B2)
              (list
                (list "X"  "X"  "O"
                      "O"  "X"  "O"
                      "X"  "X"  "X")
                (list "X"  "X"  "O"
                      "O"  "X"  "O"
                      "X"  "O"  "X")))
(check-expect (all-boards B3)
              (list
                (list "X" "O" "X"
                      "O" "O" "X"
                      "X" "X" "X")
                (list "X" "O" "X"
                      "O" "O" "X"
                      "X" "X" "O")
                (list "X" "O" "X"
                      "O" "O" "O"
                      "X" "X" "X")
                (list "X" "O" "X"
                      "O" "O" "O"
                      "X" "X" "O")))

;; PROBLEM 3:
;;
;; Now adapt your solution to filter out the boards that are impossible if
;; X and O are alternating turns. You can continue to assume that they keep
;; filling the board after someone has won though.
;;
;; You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;;
;; NOTE: make sure you keep a copy of your solution from problem 2 to answer
;; the questions on edX.


;; (listof Board) -> (listof Board)
;; Filter out the boards that are impossible if X and O are alternating turns.
;; Assumption: X plays first

(define (filter-boards lobd)
  (local
    [(define (count# elem lst)
       (foldr
         (lambda (lst-elem total)
           (if (string=? elem lst-elem)
             (add1 total)
             total))
         0
         lst))]
  (filter
    (lambda (board)
      (and (= 5 (count# "X" board)) (= 4 (count# "O" board))))
    lobd)))


(check-expect (filter-boards (all-boards B2))
              (list
                (list "X" "X" "O"
                      "O" "X" "O"
                      "X" "O" "X")))
(check-expect (filter-boards (all-boards B3))
              (list
                (list "X" "O" "X"
                      "O" "O" "X"
                      "X" "X" "O")
                (list "X" "O" "X"
                      "O" "O" "O"
                      "X" "X" "X")))
