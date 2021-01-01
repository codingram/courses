#lang htdp/bsl

;; ==============================================================================
;; PROBLEM:
;;
;; As we learned in the cat world programs, cats have a mind of their own. When they
;; reach the edge they just keep walking out of the window.
;;
;; Cows on the other hand are docile creatures. They stay inside the fence, walking
;; back and forth nicely.
;;
;; Design a world program with the following behaviour:
;;   - A cow walks back and forth across the screen.
;;   - When it gets to an edge it changes direction and goes back the other way
;;   - When you start the program it should be possible to control how fast a
;;     walker your cow is.
;;   - Pressing space makes it change direction right away.
;;
;; To help you here are two pictures of the right and left sides of a lovely cow that
;; was raised for us at Brown University. (from images/cow-image.rkt)
;;
;; Once your program works here is something you can try for fun. If you rotate the
;; images of the cow slightly, and you vary the image you use as the cow moves, you
;; can make it appear as if the cow is waddling as it walks across the screen.
;;
;; Also, to make it look better, arrange for the cow to change direction when its
;; nose hits the edge of the window, not the center of its body.
;; ==============================================================================

(require 2htdp/image)
(require 2htdp/universe)
(require "images/cow.rkt")

;; FORWARD-COW-IMG, REVERSE-COW-IMG

;; =============
;; CONSTANTS

(define WIDTH 500)
(define HEIGHT 300)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define COW-Y (/ HEIGHT 2))
(define CLEARANCE (/ (image-width FORWARD-COW-IMG) 2))


;; ============
;; DATA DEFINITIONS

(define-struct cow (x dx))
;; Cow is (make-cow Natural[0, WIDTH] Integer)
;; interp. (make-cow x dx) is a cow with x coordinate and velocity dx


;; ============
;; FUNCTION DEFINITIONS

;; Cow -> Cow
;; main function of the program; starts with (main (make-cow CLEARANCE 3))

(define (main c)
  (big-bang c                  ;Cow
    (on-tick move-cow)         ;Cow -> Cow
    (to-draw render-cow)       ;Cow -> Image
    (on-key handle-key)))      ;Cow KeyEvent -> Cow


;; Cow -> Cow
;; Moves the cow image with dx pixel to left or right

(define (move-cow c)
  (cond [(> (+ (cow-x c) (cow-dx c) CLEARANCE) WIDTH)
         (make-cow (- WIDTH CLEARANCE) (- (cow-dx c)))]
        [(< (+ (cow-x c) (cow-dx c) (- CLEARANCE)) 0)
         (make-cow CLEARANCE (- (cow-dx c)))]
        [else
         (make-cow (+ (cow-x c) (cow-dx c)) (cow-dx c))]))

;; Cow -> Image
;; Places the image of the cow in the direction determined by it's velocity

(define (render-cow c)
  (if (>= (cow-dx c) 0)
      (place-image FORWARD-COW-IMG (cow-x c) COW-Y BACKGROUND)
      (place-image REVERSE-COW-IMG (cow-x c) COW-Y BACKGROUND)))

;; Cow KeyEvent -> Cow
;; Changes the direction of the cow if spacebar is pressed

(define (handle-key c ke)
  (cond [(key=? ke " ") (make-cow (cow-x c) (- (cow-dx c)))]
        [else (make-cow (cow-x c) (cow-dx c))]))

;; =========
;; TESTS

;; Middle cases
(define C1 (make-cow (/ WIDTH 2) 3))
(define C2 (make-cow (/ WIDTH 2) -3))

;; Edge cases
(define C3 (make-cow (- (- WIDTH CLEARANCE) 3) 3))
(define C4 (make-cow (+ 3 CLEARANCE) -3))

;; Cases where the image will turn
(define C5 (make-cow (- (- WIDTH CLEARANCE) 1) 3))
(define C6 (make-cow (+ 1 CLEARANCE) -3))

;; No velocity cases
(define C7 (make-cow (- WIDTH CLEARANCE) 0))
(define C8 (make-cow CLEARANCE 0))

(check-expect (move-cow C1) (make-cow (+ (/ WIDTH 2) 3) 3))
(check-expect (move-cow C2) (make-cow (- (/ WIDTH 2) 3) -3))
(check-expect (move-cow C3) (make-cow (- WIDTH CLEARANCE) 3))
(check-expect (move-cow C4) (make-cow CLEARANCE -3))
(check-expect (move-cow C5) (make-cow (- WIDTH CLEARANCE) -3))
(check-expect (move-cow C6) (make-cow CLEARANCE 3))

(check-expect (render-cow C1) (place-image FORWARD-COW-IMG (cow-x C1) COW-Y BACKGROUND))
(check-expect (render-cow C2) (place-image REVERSE-COW-IMG (cow-x C2) COW-Y BACKGROUND))
(check-expect (render-cow C3) (place-image FORWARD-COW-IMG (cow-x C3) COW-Y BACKGROUND))
(check-expect (render-cow C4) (place-image REVERSE-COW-IMG (cow-x C4) COW-Y BACKGROUND))
(check-expect (render-cow C5) (place-image FORWARD-COW-IMG (cow-x C5) COW-Y BACKGROUND))
(check-expect (render-cow C6) (place-image REVERSE-COW-IMG (cow-x C6) COW-Y BACKGROUND))
(check-expect (render-cow C7) (place-image FORWARD-COW-IMG (cow-x C7) COW-Y BACKGROUND))
(check-expect (render-cow C8) (place-image FORWARD-COW-IMG (cow-x C8) COW-Y BACKGROUND))

(check-expect (handle-key C1 " ") (make-cow (cow-x C1) (- (cow-dx C1))))
(check-expect (handle-key C2 " ") (make-cow (cow-x C2) (- (cow-dx C2))))
(check-expect (handle-key C3 "a") (make-cow (cow-x C3) (cow-dx C3)))
(check-expect (handle-key C4 "b") (make-cow (cow-x C4) (cow-dx C4)))
(check-expect (handle-key C5 " ") (make-cow (cow-x C5) (- (cow-dx C5))))
(check-expect (handle-key C6 "a") (make-cow (cow-x C6) (cow-dx C6)))

;; Comment this out to run the program on the terminal
;; (main (make-cow CLEARANCE 5))
