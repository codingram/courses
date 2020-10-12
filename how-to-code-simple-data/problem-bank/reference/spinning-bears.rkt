#lang htdp/bsl

#| PROBLEM:
 |
 | In this problem you will design another world program. In this program the changing
 | information will be more complex - your type definitions will involve arbitrary
 | sized data as well as the reference rule and compound data. But by doing your
 | design in two phases you will be able to manage this complexity. As a whole, this problem
 | will represent an excellent summary of the material covered so far in the course, and world
 | programs in particular.
 |
 | This world is about spinning bears. The world will start with an empty screen. Clicking
 | anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
 | but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the
 | screen, a new upright bear appears and starts spinning.
 |
 | So each bear has its own x and y position, as well as its angle of rotation. And there are an
 | arbitrary amount of bears.
 |
 | To start, design a world that has only one spinning bear. Initially, the world will start
 | with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
 | world will replace the old bear with a new bear at the new spot. You can do this part
 | with only material up through compound.
 |
 | Once this is working you should expand the program to include an arbitrary number of bears.
 |
 | Here is an image of a bear for you to use: images/bear.rkt |#

(require 2htdp/image)
(require 2htdp/universe)
(require "images/bear.rkt")


; Constants

(define WIDTH 800)
(define HEIGHT 600)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define ROTATIONAL-SPEED 3)


; Data definitions

(define-struct bear (x y angle))
; Bear is (make-bear Number Number Natural(0, 360])
; interp. the x and y coordinate of the bear image and
;         the current angular rotation in degress
#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-angle b)))

; ListOfBear is one of:
; - empty
; - (cons Bear ListOfBear)
; interp. the list of bears

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
          (... (fn-for-bear (first lob))
               (fn-for-lob (rest lob)))]))


; Function definitions

; ListOfBear -> ListOfBear
; Main function of the program, starts with (main empty)

(define (main lob)
  (big-bang
    lob                          ; ListOfBear
    (on-tick change-bears)       ; ListOfBear -> ListOfBear
    (to-draw render-bears)       ; ListOfBear -> Image
    (on-mouse handle-mouse)))    ; ListOfBear x y me -> ListOfBear

; Bear -> Bear
; Rotates the bear with ROTATIONAL-SPEED
; Helper function for change-bears

(define (rotate-bear b)
  (make-bear
    (bear-x b) (bear-y b)
    (+ (bear-angle b) ROTATIONAL-SPEED)))

; ListOfBear -> ListOfBear
; Rotates each bear in the list with ROTATIONAL-SPEED

(define (change-bears lob)
  (cond [(empty? lob) empty]
        [else
          (cons (rotate-bear (first lob))
                (change-bears (rest lob)))]))

; Bear -> Image
; Produces the image of a single bear rotated at the given angle

(define (bear-image b)
  (rotate (bear-angle b) BEAR))

; ListOfBear -> Image
; Produces the image of all the bears on BACKGROUND at the given data
; of x and y coordinate and the angle for each bear

(define (render-bears lob)
  (cond [(empty? lob) BACKGROUND]
        [else
          (place-image (bear-image (first lob))
                       (bear-x (first lob))
                       (bear-y (first lob))
                       (render-bears (rest lob)))]))

; ListOfBear x y me -> ListOfBear
; Adds another bear at the x and y coordinate of the mouse click
; to the given list of bears

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-bear x y 0) lob)]
        [else lob]))


; Tests

(define B1 (make-bear 50 50 10))
(define B2 (make-bear 70 30 0))
(define B3 (make-bear 130 170 45))

(define LOB1 empty)
(define LOB2 (cons B1 empty))
(define LOB3 (cons B2 (cons B3 empty)))

(check-expect (rotate-bear B1) (make-bear 50 50 (+ 10 ROTATIONAL-SPEED)))
(check-expect (rotate-bear B2) (make-bear 70 30 ROTATIONAL-SPEED))
(check-expect (rotate-bear B3) (make-bear 130 170 (+ 45 ROTATIONAL-SPEED)))

(check-expect (change-bears LOB1) empty)
(check-expect (change-bears LOB2) (cons (make-bear 50 50 (+ 10 ROTATIONAL-SPEED)) empty))
(check-expect (change-bears LOB3)
              (cons (make-bear 70 30 ROTATIONAL-SPEED)
                    (cons (make-bear 130 170 (+ 45 ROTATIONAL-SPEED)) empty)))

(check-expect (bear-image B1) (rotate 10 BEAR))
(check-expect (bear-image B2) BEAR)
(check-expect (bear-image B3) (rotate 45 BEAR))

(check-expect (render-bears LOB1) BACKGROUND)
(check-expect (render-bears LOB2)
              (place-image (rotate 10 BEAR) 50 50 BACKGROUND))
(check-expect (render-bears LOB3)
              (place-image BEAR 70 30 (place-image (rotate 45 BEAR) 130 170 BACKGROUND)))

(check-expect (handle-mouse LOB1 50 100 "button-down")
              (cons (make-bear 50 100 0) LOB1))
(check-expect (handle-mouse LOB1 40 70 "move") LOB1)
(check-expect (handle-mouse LOB2 100 140 "button-down")
              (cons (make-bear 100 140 0) LOB2))
(check-expect (handle-mouse LOB2 120 160 "button-up") LOB2)
(check-expect (handle-mouse LOB3 150 200 "button-down")
              (cons (make-bear 150 200 0) LOB3))
(check-expect (handle-mouse LOB3 140 250 "enter") LOB3)

; Uncomment the below line to start the program
; (main empty)
