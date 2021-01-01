#lang htdp/bsl

#| PROBLEM:
 
Design a function called decreasing-image that consumes a Natural n 
and produces an image of all the numbers from n to 0 side by side. 
 
So (decreasing-image 3) should produce: run the program |#

(require 2htdp/image)


;; Constants

(define TEXT-SIZE 30)
(define TEXT-COLOR "white")


;; Functions

;; Number -> Image
;; Converts a number into an equivalent image
;; Helper function for decreasing-image

(define (number->image num)
  (text (number->string num) TEXT-SIZE TEXT-COLOR))

;; Natural -> Image
;; Produces an image of all the numbers from n to 0 side by side

(define (decreasing-image n)
  (cond [(zero? n) (number->image 0)]
        [else
         (beside (number->image n)
                 (decreasing-image (sub1 n)))]))


;; Tests

(check-expect (number->image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (number->image 1) (text "1" TEXT-SIZE TEXT-COLOR))

(check-expect (decreasing-image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (decreasing-image 1) (beside (text "1" TEXT-SIZE TEXT-COLOR)
                                           (text "0" TEXT-SIZE TEXT-COLOR)))
(check-expect (decreasing-image 2) (beside (text "2" TEXT-SIZE TEXT-COLOR)
                                           (text "1" TEXT-SIZE TEXT-COLOR)
                                           (text "0" TEXT-SIZE TEXT-COLOR)))
