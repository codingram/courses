#lang htdp/bsl

#| PROBLEM:
 |
 | In this problem, we will design an animation of throwing a water balloon.
 | When the program starts the water balloon should appear on the left side
 | of the screen, half-way up.  Since the balloon was thrown, it should
 | fly across the screen, rotating in a clockwise fashion. Pressing the
 | space key should cause the program to start over with the water balloon
 | back at the left side of the screen.
 |
 | NOTE: Please include your domain analysis at the top in a comment box.
 |
 | Here is an image of the water balloon:
 | From images/water-balloon.rkt
 |
 | NOTE: The rotate function wants an angle in degrees as its first
 | argument. By that it means Number[0, 360). As time goes by your balloon
 | may end up spinning more than once, for example, you may get to a point
 | where it has spun 362 degrees, which rotate won't accept.
 |
 | The solution to that is to use the modulo function as follows:
 |
 | (rotate (modulo ... 360) (text "hello" 30 "black"))
 |
 | where ... should be replaced by the number of degrees to rotate.
 |
 | NOTE: It is possible to design this program with simple atomic data,
 | but we would like you to use compound data. |#

(require 2htdp/image)
(require 2htdp/universe)
(require "images/water-balloon.rkt")

;; Constants

(define WIDTH 800)
(define HEIGHT 300)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define BALLOON-Y (/ HEIGHT 2))
(define LINEAR-SPEED 2)
(define ANGULAR-SPEED 2)


;; Data definitions

(define-struct wb (pos angle))
;; Wb is (make-wb Natural Natural[0, 360))
;; interp. x coordinate and the angle for the water balloon


;; Function definitions

;; Wb -> Wb
;; main function of the program, starts with (main (make-wb 0 0))

(define (main b)
  (big-bang
    b                             ; Wb
    (state true)
    (on-tick move-balloon)        ; Wb -> Wb
    (to-draw render-balloon)      ; Wb -> Image
    (on-key handle-key)))         ; Wb KeyEvent -> Wb

;; Wb -> Wb
;; Increases the x coordinate and angle for the water balloon according to
;; LINEAR-SPEED and ANGULAR-SPEED

(define (move-balloon b)
  (make-wb
    (+ (wb-pos b) LINEAR-SPEED)
    (+ (wb-angle b) (- ANGULAR-SPEED))))

;; Wb -> Image
;; Produces the image of the water balloon on BACKGROUND according to the
;; data provided

(define (render-balloon b)
  (place-image
    (rotate (modulo (wb-angle b) 360) WATER-BALLOON)
    (wb-pos b) BALLOON-Y
    BACKGROUND))

;; Wb KeyEvent -> Wb
;; Resets the water balloon to its initial state when spacebar is pressed
;; otherwise it does nothing

(define (handle-key b ke)
  (cond [(key=? ke " ") (make-wb 0 0)]
        [else b]))


;; Tests

(define WB1 (make-wb 0 0))
(define WB2 (make-wb 100 -100))
(define WB3 (make-wb 100 -359))
(define WB4 (make-wb 200 -450))

(check-expect (move-balloon WB1) (make-wb (+ 0 LINEAR-SPEED) (- (+ 0 ANGULAR-SPEED))))
(check-expect (move-balloon WB2) (make-wb (+ 100 LINEAR-SPEED) (- (+ 100 ANGULAR-SPEED))))
(check-expect (move-balloon WB3) (make-wb (+ 100 LINEAR-SPEED) (- (+ 359 ANGULAR-SPEED))))
(check-expect (move-balloon WB4) (make-wb (+ 200 LINEAR-SPEED) (- (+ 450 ANGULAR-SPEED))))

(check-expect (render-balloon WB4)
              (place-image
                (rotate 270 WATER-BALLOON)
                (wb-pos WB4) BALLOON-Y BACKGROUND))

(check-expect (handle-key WB1 " ") (make-wb 0 0))
(check-expect (handle-key WB2 "a") WB2)
(check-expect (handle-key WB3 " ") (make-wb 0 0))

;; Uncomment the below line to run the program
;; (main (make-wb 0 0))
