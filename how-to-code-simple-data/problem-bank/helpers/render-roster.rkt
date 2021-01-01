#lang htdp/bsl

#| Problem:
 |
 | You are running a dodgeball tournament and are given a list of all
 | of the players in a particular game as well as their team numbers.
 | You need to build a game roster like the one shown below. We've given
 | you some constants and data definitions for Player, ListOfPlayer
 | and ListOfString to work with.
 |
 | While you're working on these problems, make sure to keep your
 | helper rules in mind and use helper functions when necessary.
 |
 | Team 1      Team 2
 | Samael      Georgia
 | John        Sally
 | Brenda      Bob
 | Alice       Carl
 | Trish       Phil
 | Tom         Ellen |#


(require 2htdp/image)


;; Constants

(define CELL-WIDTH 200)
(define CELL-HEIGHT 30)
(define TEXT-SIZE 20)
(define TEXT-COLOR "white")


;; Data Definitions

(define-struct player (name team))
;; Player is (make-player String Natural[1,2])
;; interp. a dodgeball player.
;;   (make-player s t) represents the player named s
;;   who plays on team t

#;
(define (fn-for-player p)
  (... (player-name p)
       (player-team p)))


;; ListOfPlayer is one of:
;; - empty
;; - (cons Player ListOfPlayer)
;; interp.  A list of players.
#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-player (first lop))
              (fn-for-lop (rest lop)))]))


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))


;; Functions

#| PROBLEM 1:
 |
 | Design a function called select-players that consumes a list
 | of players and a team t (Natural[1,2]) and produces a list of
 | players that are on team t. |#


;; ListOfPlayer Natural -> ListOfPlayer
;; Produces the list of players which are on team t[1, 2]

(define (select-players lop t)
  (cond [(empty? lop) empty]
        [else
         (if (on-team? (first lop) t)
             (cons (first lop) (select-players (rest lop) t))
             (select-players (rest lop) t))]))

;; Player -> Boolean
;; Determines whether the player is in team t or not

(define (on-team? p t)
  (= t (player-team p)))


#| PROBLEM 2:
 |
 | Complete the design of render-roster. We've started you off with
 | the signature, purpose, stub and examples. You'll need to use
 | the function that you designed in Problem 1.
 |
 | Note that we've also given you a full function design for render-los
 | and its helper, render-cell. You will need to use these functions
 | when solving this problem. |#

;; ListOfPlayer -> Image
;; Render a game roster from the given list of players

(define (render-roster lop)
  (beside/align "top"
                (render-team (select-players lop 1) 1)
                (render-team (select-players lop 2) 2)))

;; ListOfPlayer Natural[1, 2] -> Image
;; Produces the image column for given list of players for the provided team t

(define (render-team lop t)
  (above (render-cell (string-append "Team " (number->string t)))
         (render-los (player-names lop))))

;; ListOfPlayer -> ListOfString
;; Produces a list of string (player names) from list of players

(define (player-names lop)
  (cond [(empty? lop) empty]
        [else
         (cons (player-name (first lop))
               (player-names (rest lop)))]))

;; ListOfString -> Image
;; Render a list of strings as a column of cells.

(define (render-los los)
  (cond [(empty? los) empty-image]
        [else
         (above (render-cell (first los))
                (render-los (rest los)))]))

;; String -> Image
;; Render a cell of the game table

(define (render-cell s)
  (overlay
   (text s TEXT-SIZE TEXT-COLOR)
   (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))


;; Tests

(define P11 (make-player "Samael" 1))
(define P12 (make-player "John" 1))
(define P13 (make-player "Brenda" 1))
(define P14 (make-player "Alice" 1))
(define P15 (make-player "Trish" 1))
(define P16 (make-player "Tom" 1))
(define P21 (make-player "Georgia" 2))
(define P22 (make-player "Sally" 2))
(define P23 (make-player "Bob" 2))
(define P24 (make-player "Carl" 2))
(define P25 (make-player "Phil" 2))
(define P26 (make-player "Ellen" 2))

(define LOP1 empty)
(define LOP2 (cons P11 empty))
(define LOP3 (cons P12 (cons P13 (cons P14 empty))))
(define LOP4 (cons P15 (cons P22 (cons P13 (cons P11 (cons P25 (cons P26 empty)))))))
(define LOP5 (cons P11 (cons P21 empty)))
(define LOP6 (cons P21 (cons P22 empty)))

(check-expect (on-team? P14 1) true)
(check-expect (on-team? P12 2) false)
(check-expect (on-team? P22 1) false)
(check-expect (on-team? P24 2) true)

(check-expect (select-players LOP1 1) empty)
(check-expect (select-players LOP2 1) LOP2)
(check-expect (select-players LOP3 2) empty)  ; No player in team 2 in LOP3
(check-expect (select-players LOP4 1) (cons P15 (cons P13 (cons P11 empty))))
(check-expect (select-players LOP4 2) (cons P22 (cons P25 (cons P26 empty))))

(check-expect (render-roster empty)
              (beside/align
               "top"
               (overlay
                (text "Team 1" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
               (overlay
                (text "Team 2" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))))

(check-expect (render-roster LOP5)
              (beside/align
               "top"
               (above
                (overlay
                 (text "Team 1" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
                (overlay
                 (text "Samael" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))
               (above
                (overlay
                 (text "Team 2" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
                (overlay
                 (text "Georgia" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))))

(check-expect (render-team empty 1) (render-cell "Team 1"))
(check-expect (render-team LOP6 2)
              (above (render-cell "Team 2")
                     (render-cell "Georgia")
                     (render-cell "Sally")))
(check-expect (render-team LOP3 1)
              (above (render-cell "Team 1")
                     (render-cell "John")
                     (render-cell "Brenda")
                     (render-cell "Alice")))

(check-expect (player-names LOP1) empty)
(check-expect (player-names LOP5) (cons "Samael" (cons "Georgia" empty)))
(check-expect (player-names LOP6) (cons "Georgia" (cons "Sally" empty)))

(check-expect (render-los empty) empty-image)
(check-expect (render-los (cons "Samael" empty))
              (above
               (overlay
                (text "Samael" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
               empty-image))
(check-expect (render-los (cons "Samael" (cons "Brigid" empty)))
              (above
               (overlay
                (text "Samael" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
               (overlay
                (text "Brigid" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))))

(check-expect (render-cell "Team 1")
              (overlay
               (text "Team 1" TEXT-SIZE TEXT-COLOR)
               (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))

;; Uncomment the below line to run the program
#| (render-roster (cons P11
                     (cons P12
                           (cons P13
                                 (cons P14
                                       (cons P15
                                             (cons P16
                                                   (cons P21
                                                         (cons P22
                                                               (cons P23
                                                                     (cons P24
                                                                           (cons P25
                                                                                 (cons P26 empty))))))))))))) |#
