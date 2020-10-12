#lang htdp/bsl

; ======================================================================
; PROBLEM:
;
; Design a world program as follows:
;
; The world starts off with a lambda on the left hand side of the screen. As
; time passes, the lambda will roll towards the right hand side of the screen.
; Clicking the mouse changes the direction the lambda is rolling (ie from
; left -> right to right -> left). If the lambda hits the side of the window
; it should also change direction.
;
; NOTE 1: Remember to follow the HtDW recipe! Be sure to do a proper domain
; analysis before starting to work on the code file.
;
; NOTE 2: DO THIS PROBLEM IN STAGES.
;
; FIRST
;
; Just make the lambda slide back and forth across the screen without rolling.
;
; SECOND
;
; Make the lambda spin as it slides, but don't worry about making the spinning
; be just exactly right to make it look like its rolling. Just have it
; spinning and sliding back and forth across the screen.
;
; FINALLY
;
; Work out the math you need to in order to make the lambda look like it is
; actually rolling.  Remember that the circumference of a circle is 2*pi*radius,
; so that for each degree of rotation the circle needs to move:
;
;    2*pi*radius
;    -----------
;        360
;
; Also note that the rotate function requires an angle in degrees as its
; first argument. [By that it means Number[0, 360). As time goes by the lambda
; may end up spinning more than once, for example, you may get to a point
; where it has spun 362 degrees, which rotate won't accept. One solution to
; that is to  use the modulo function as follows:
;
; (rotate (modulo ... 360) LAMBDA)
;
; where ... can be an expression that produces any positive number of degrees
; and remainder will produce a number in [0, 360).
;
; Remember that you can lookup the documentation of modulo if you need to know
; more about it.
; ======================================================================

(require 2htdp/image)
(require 2htdp/universe)
(require "images/lambda.rkt")

; LAMBDA

; =================
; Constants:

(define ROTATIONAL-SPEED -5)
(define RADIUS (/ (image-height LAMBDA) 2))
(define APPROX-PI 3.14159)
(define CIRCUMFERENCE (* 2 APPROX-PI RADIUS))
(define X-PER-DEGREE (/ CIRCUMFERENCE 360))
(define X-SPEED (* -1 ROTATIONAL-SPEED X-PER-DEGREE))
(define HEIGHT (* 3 RADIUS))
(define WIDTH (* 3 CIRCUMFERENCE))
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define LAMBDA-Y (- HEIGHT RADIUS))

; =================
; Data definitions:

(define-struct lbd (direction pos angle))
; Lbd is (make-lbd Integer Natural[0, (WIDTH - LAMBDA-RADIUS)] Natural[0, 360))
; interp. direction -> direction of the roll (-1 for anticlockwise, +1 for clockwise)
;         pos -> x coordinate of LAMBDA
;         angle -> current angle of LAMBDA, in range [0, 360)


; ================
; Function definitions:

; Lbd -> Lbd
; main function of the program; starts with (main (make-lbd 1 0 0))

(define (main l)
  (big-bang l                    ; Lbd
    (on-tick move-lambda)        ; Lbd -> Lbd
    (to-draw render-lambda)      ; Lbd -> Image
    (on-mouse handle-mouse)))    ; Lbd x y me -> Lbd

; Lbd -> Lbd
; Increase the x coordinate and angle for the image LAMBDA if within WIDTH
; otherwise change the direction

(define (move-lambda l)
  (cond [(or (> (+ (lbd-pos l) (* (lbd-direction l) X-SPEED) RADIUS) (- WIDTH RADIUS))
        (< (+ (lbd-pos l) (* (lbd-direction l) X-SPEED) RADIUS) RADIUS))
         (make-lbd (- (lbd-direction l)) (lbd-pos l) (lbd-angle l))]
        [else (make-lbd
                (lbd-direction l)
                (+ (lbd-pos l) (* (lbd-direction l) X-SPEED))
                (+ (lbd-angle l) (* (lbd-direction l) ROTATIONAL-SPEED)))]))

; Lbd -> Image
; Produces the image of LAMBDA on BACKGROUND from given data

(define (render-lambda l)
  (place-image
    (rotate (modulo (lbd-angle l) 360) LAMBDA)
    (+ (lbd-pos l) RADIUS) LAMBDA-Y BACKGROUND))

; Lbd x y me -> Lbd
; Reverse the rolling direction of LAMBDA on mouse click

(define (handle-mouse l x y me)
  (cond [(mouse=? me "button-down")
         (make-lbd (- (lbd-direction l)) (lbd-pos l) (lbd-angle l))]
        [else l]))


; ==================
; Tests:

(define L1 (make-lbd 1 100 100))
(define L2 (make-lbd -1 100 100))

(define L3 (make-lbd 1 (- WIDTH RADIUS) 100))
(define L4 (make-lbd -1 0 100))

(check-expect (move-lambda L1) (make-lbd 1 (+ 100 X-SPEED) (+ 100 ROTATIONAL-SPEED)))
(check-expect (move-lambda L2) (make-lbd -1 (- 100 X-SPEED) (- 100 ROTATIONAL-SPEED)))
(check-expect (move-lambda L3) (make-lbd -1 (lbd-pos L3) (lbd-angle L3)))
(check-expect (move-lambda L4) (make-lbd 1 (lbd-pos L4) (lbd-angle L4)))

(check-expect (render-lambda L1) (place-image (rotate 100 LAMBDA)
                                              (+ 100 RADIUS) LAMBDA-Y
                                              BACKGROUND))

(check-expect (handle-mouse L1 100 100 "move") L1)
(check-expect (handle-mouse L2 100 100 "button-down")
              (make-lbd 1 100 100))

; Uncomment the below line to run the program
; (main (make-lbd 1 0 0))
