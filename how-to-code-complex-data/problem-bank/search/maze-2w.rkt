#lang htdp/asl

(require 2htdp/image)


;; In this problem set you will design a program to check whether a given simple maze is
;; solvable.  Note that you are operating on VERY SIMPLE mazes, specifically:
;;
;;    - all of your mazes will be square
;;    - the maze always starts in the upper left corner and ends in the lower right corner
;;    - at each move, you can only move down or right
;;
;; Design a representation for mazes, and then design a function that consumes a maze and
;; produces true if the maze is solvable, false otherwise.
;;
;; Solvable means that it is possible to start at the upper left, and make it all the way to
;; the lower right.  Your final path can only move down or right one square at a time. BUT, it
;; is permissible to backtrack if you reach a dead end.
;;
;; For example, the first three mazes below are solvable.  Note that the fourth is not solvable
;; because it would require moving left. In this solver you only need to support moving down
;; and right! Moving in all four directions introduces complications we are not yet ready for.
;;
;;
;; Your function will of course have a number of helpers. Use everything you have learned so far
;; this term to design this program.
;;
;; One big hint. Remember that we avoid using an image based representation of information unless
;; we have to. So the above are RENDERINGs of mazes. You should design a data definition that
;; represents such mazes, but don't use images as your representation.
;;
;; For extra fun, once you are done, design a function that consumes a maze and produces a
;; rendering of it, similar to the above images.



;; Solve simple square mazes


;; -------------------
;; Data definitions:

;; MazeBlock is Natural[0, 1]
;; interp. 1 represents a block which is a path and can be travelled onto
;;         0 represents a block which is not a path and so cannot be travelled onto

;; Maze is (listof MazeBlock)
;; interp. a list of MazeBlock which represents the entire Maze path
;;         As it is assumed that the maze is going to be square only, the number of
;;         MazeBlock in the list should be a perfect square. The side length will be
;;         (sqrt (length <Maze>))

(define-struct position (x y))
;; Position is (make-position Natural Natural)
;; interp. Position of a MazeBlock in a given Maze
;;         (<= 0 x (sub1 <length>)) and (<= 0 y (sub1 <length>))


;; --------------
;; Constants:

(define UNSOLVABLE-MAZE1
  (list 1 0 0 0
        0 0 0 0
        1 1 0 0
        0 1 0 0))

(define UNSOLVABLE-MAZE2
  (list 1 1 1 1 0
        1 0 0 1 0
        1 0 1 1 0
        1 0 1 0 0
        1 0 1 1 1))

(define SOLVABLE-MAZE1
  (list 1 1 0 0
        0 1 1 0
        0 0 1 0
        0 0 1 1))

(define SOLVABLE-MAZE2
  (list 1 0 0 0 0
        1 1 1 1 0
        0 1 0 1 0
        0 1 1 0 0
        0 0 1 1 1))


;; --------------
;; Functions:

;; Maze -> Boolean
;; Checks whether the maze can be solvable or not meaning is there a path which can be
;; travelled from the top left corner to the bottom right corner where the condition is
;; we can only move in the bottom and right direction.

(define (solvable? maze)
  (local
    [
     ; Position -> Boolean
     (define (helper-maze current-position)
       (or (solved? maze current-position)
           (helper-list-of-maze (next-position maze current-position))))

     ; (listof Position) -> Boolean
     (define (helper-list-of-maze list-of-position)
       (cond [(empty? list-of-position) false]
             [else
               (or (helper-maze (first list-of-position))
                   (helper-list-of-maze (rest list-of-position)))]))]

    (helper-maze (make-position 0 0))))


;; Maze Position -> Boolean
;; Determines whether the maze is solved when we are at the given Position

(define (solved? maze current-position)
  (letrec ([boundary (sub1 (sqrt (length maze)))])
    (= (position-x current-position)
       (position-y current-position)
       boundary)))


;; Maze Position -> (listof Maze)
;; Provides the list of maze with the current position moved to bottom and right,
;; filtering out the ones which are not possible.

(define (next-position maze current-position)
  (local
    [(define boundary (sub1 (sqrt (length maze))))

      ; Position -> (listof Position)
      ; Returns the next positions from the current Position
      (define (next-positions current-position)
        (list (make-position (position-x current-position)
                             (add1 (position-y current-position)))
              (make-position (add1 (position-x current-position))
                             (position-y current-position))))

     ; Position -> Boolean
     ; Determines whether a position is valid in the given maze
     ; This works on the short circuit behavior where we first check whether the
     ; x and y coordinate are within the maze boundary and then check whether the
     ; current position is a valid maze path.
     (define (valid-position? current-position)
       (and (<= (position-x current-position) boundary)
            (<= (position-y current-position) boundary)
            (= (pos->value maze current-position) 1)))]
  (filter valid-position? (next-positions current-position))))


;; Maze Position -> MazeBlock
;; Return the value of the MazeBlock at the given Position in the Maze

(define (pos->value maze position)
  (letrec ([len (sqrt (length maze))])
    (list-ref maze (+ (position-x position) (* (position-y position) len)))))


;; Maze -> Image
;; Renders the given Maze into an image
;; TODO
(define (maze->image maze) empty-image)

;; ---------------
;; Tests:

(define P00 (make-position 0 0))
(define P12 (make-position 1 2))
(define P21 (make-position 2 1))
(define P11 (make-position 1 1))
(define P33 (make-position 3 3))

(check-expect (solvable? UNSOLVABLE-MAZE1) false)
(check-expect (solvable? UNSOLVABLE-MAZE2) false)
(check-expect (solvable? SOLVABLE-MAZE1) true)
(check-expect (solvable? SOLVABLE-MAZE2) true)

(check-expect (solved? UNSOLVABLE-MAZE1 P00) false)
(check-expect (solved? SOLVABLE-MAZE1 P33) true)

(check-expect (next-position UNSOLVABLE-MAZE1 P00) empty)
(check-expect (next-position SOLVABLE-MAZE1 P00) (list (make-position 1 0)))
(check-expect (next-position SOLVABLE-MAZE2 P00) (list (make-position 0 1)))
(check-expect (next-position SOLVABLE-MAZE2 P11) (list P12 P21))
(check-expect (next-position SOLVABLE-MAZE2 (make-position 2 4))
              (list (make-position 3 4)))

(check-expect (pos->value UNSOLVABLE-MAZE1 P00) 1)
(check-expect (pos->value SOLVABLE-MAZE1 P12) 0)
