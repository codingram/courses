#lang htdp/bsl

;; ==============================================================================
;; PROBLEM:
;;
;; Design a world program as follows:
;;
;; The world starts off with a small square at the center of the screen. As time
;; passes, the square stays fixed at the center, but increases in size and rotates
;; at a constant speed.Pressing the spacebar resets the world so that the square
;; is small and unrotated.
;;
;; NOTE 1: Remember to follow the HtDW recipe! Be sure to do a proper domain
;; analysis before starting to work on the code file.
;;
;; NOTE 2: The rotate function requires an angle in degrees as its first
;; argument. By that it means Number[0, 360). As time goes by the box may end up
;; spinning more than once, for example, you may get to a point where it has spun
;; 362 degrees, which rotate won't accept. One solution to that is to use the
;; remainder function as follows:
;;
;; (rotate (remainder ... 360) (text "hello" 30 "black"))
;;
;; where ... can be an expression that produces any positive number of degrees
;; and remainder will produce a number in [0, 360).
;;
;; Remember that you can lookup the documentation of rotate if you need to know
;; more about it.
;;
;; NOTE 3: There is a way to do this without using compound data. But you should
;; design the compound data based solution.
;; ==============================================================================

(require 2htdp/image)
(require 2htdp/universe)

;; ============
;; CONSTANTS

(define WIDTH 500)
(define HEIGHT WIDTH)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define GROWTH-SPEED 3)
(define ROTATIONAL-SPEED 15)
(define BOX-COLOR "red")
(define BOX-MODE "solid")


;; ============
;; Data definition

(define-struct box (length angle))
;; Box is (make-square Natural Natural[0, 360))
;; interp. the box with the following properties:
;;   - length: Natural, length of the square
;;   - angle: Natural[0, 360), angle of rotation of the square


;; ============
;; Function definition

;; Box -> Box
;; main function of the program; starts with (main (make-box 0 0))

(define (main box)
  (big-bang box                   ; Box
    (on-tick change-square)       ; Box -> Box
    (to-draw render-square)       ; Box -> Image
    (on-key handle-key)))         ; Box KeyEvent -> Box


;; Box -> Box
;; produce the box with increased length and angle

(define (change-square b)
  (make-box
   (+ (box-length b) GROWTH-SPEED)
   (+ (box-angle b) ROTATIONAL-SPEED)))


;; Box -> Image
;; produces the image of the box with length and angle to display

(define (render-square b)
  (place-image
   (rotate
    (remainder (box-angle b) 360)
    (square (box-length b) BOX-MODE BOX-COLOR))
   (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))


;; Box KeyEvent -> Box
;; resets the image of the box back to it's initial state

(define (handle-key b ke)
  (cond [(key=? ke " ") (make-box 0 0)]
        [else b]))


;; ============
;; Tests

(define B1 (make-box 0 0))        ; Initial state
(define B2 (make-box 200 124))
(define B3 (make-box 334 359))
(define B4 (make-box 399 360))
(define B5 (make-box 478 560))

(define (img-util img)
  (place-image img (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(check-expect (change-square B1) (make-box (+ 0 GROWTH-SPEED) (+ 0 ROTATIONAL-SPEED)))
(check-expect (change-square B2) (make-box (+ 200 GROWTH-SPEED) (+ 124 ROTATIONAL-SPEED)))
(check-expect (change-square B3) (make-box (+ 334 GROWTH-SPEED) (+ 359 ROTATIONAL-SPEED)))
(check-expect (change-square B4) (make-box (+ 399 GROWTH-SPEED) (+ 360 ROTATIONAL-SPEED)))
(check-expect (change-square B5) (make-box (+ 478 GROWTH-SPEED) (+ 560 ROTATIONAL-SPEED)))

(check-expect (render-square B1) (img-util (rotate 0 (square 0 BOX-MODE BOX-COLOR))))
(check-expect (render-square B2) (img-util (rotate 124 (square 200 BOX-MODE BOX-COLOR))))
(check-expect (render-square B3) (img-util (rotate 359 (square 334 BOX-MODE BOX-COLOR))))
(check-expect (render-square B4) (img-util (rotate 0 (square 399 BOX-MODE BOX-COLOR))))
(check-expect (render-square B5) (img-util (rotate 200 (square 478 BOX-MODE BOX-COLOR))))

(check-expect (handle-key B1 " ") B1)
(check-expect (handle-key B2 " ") B1)
(check-expect (handle-key B3 "a") B3)
(check-expect (handle-key B4 "x") B4)
(check-expect (handle-key B5 " ") B1)
(check-expect (handle-key B3 "6") B3)
