#lang htdp/bsl

#| PROBLEM:

Design a function that consumes a natural number n and a color c, and produces n
concentric circles of the given color.

So (concentric-circles 5 "black") should produce: run the program |#

(require 2htdp/image)


;; Constants

(define DISTANCE 10)


;; Data definitions

;; Color is String
;; interp. the color of the circle


;; Function definitions

;; Natural Color -> Image
;; Produces n concentric circles each at DISTANCE apart

(define (concentric-circles n c)
  (cond [(zero? n) empty-image]
        [else
          (overlay (circle (* n DISTANCE) "outline" c)
                   (concentric-circles (sub1 n) c))]))


;; Tests

(check-expect (concentric-circles 0 "white") empty-image)
(check-expect (concentric-circles 1 "white")
              (overlay (circle DISTANCE "outline" "white")
                       empty-image))
(check-expect (concentric-circles 2 "white")
              (overlay (circle (* 2 DISTANCE) "outline" "white")
                       (circle DISTANCE "outline" "white")
                       empty-image))
(check-expect (concentric-circles 3 "red")
              (overlay (circle (* 3 DISTANCE) "outline" "red")
                       (circle (* 2 DISTANCE) "outline" "red")
                       (circle DISTANCE "outline" "red")
                       empty-image))
