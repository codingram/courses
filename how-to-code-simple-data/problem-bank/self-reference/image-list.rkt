#lang htdp/bsl

(require 2htdp/image)

; Data definitions:
;
; PROBLEM A:
;
; Design a data definition to represent a list of images. Call it ListOfImage.

; ListOfImage is one of:
; - empty
; - (cons Image ListOfImage)
; interp. a list of images

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


; Functions:
;
; PROBLEM B:
;
; Design a function that consumes a list of images and produces a number
; that is the sum of the areas of each image. For area, just use the image's
; width times its height.

; ListOfImages -> Number
; Produces the sum of areas of all the images in the given list

(define (sum-area loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-height (first loi)) (image-width (first loi)))
              (sum-area (rest loi)))]))


; Tests

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (square 40 "solid" "blue"))

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I2 (cons I1 empty)))
(define LOI4 (cons I2 (cons I1 (cons I2 empty))))

(check-expect (sum-area LOI1) 0)
(check-expect (sum-area LOI2) 200)
(check-expect (sum-area LOI3) 1800)
(check-expect (sum-area LOI4) 3400)
