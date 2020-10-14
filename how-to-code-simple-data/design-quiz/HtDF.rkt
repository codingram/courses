#lang htdp/bsl

#| PROBLEM:
 |
 | Design a function that consumes two images and produces true if the first
 | is larger than the second. |#

(require 2htdp/image)

; Image Image -> Boolean
; Produces true if the first image is larger than the second

(define (is-image-large? img1 img2)
  (> (image-area img1) (image-area img2)))

; Image -> Number
; Returns the area of the image

(define (image-area img)
  (* (image-height img) (image-width img)))

; Tests

(define I1 (rectangle 10 15 "solid" "red"))    ; 150
(define I2 (rectangle 15 20 "solid" "green"))  ; 300
(define I3 (square 8 "solid" "white"))         ; 64

(check-expect (is-image-large? I1 I2) false)
(check-expect (is-image-large? I2 I3) true)
(check-expect (is-image-large? I3 I3) false)

(check-expect (image-area I1) 150)
(check-expect (image-area I2) 300)
(check-expect (image-area I3) 64)
