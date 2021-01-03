#lang htdp/isl


;; PROBLEM A:
;;
;; Design an abstract function (including signature, purpose, and tests) to
;; simplify the two sum-of functions.


;; (listof Number) -> Number
;; produce the sum of the squares of the numbers in lon
(check-expect (sum-of-squares empty) 0)
(check-expect (sum-of-squares (list 2 4)) (+ 4 16))

(define (sum-of-squares lon)
  (sum-of sqr lon))

;; (listof String) -> Number
;; produce the sum of the lengths of the strings in los
(check-expect (sum-of-lengths empty) 0)
(check-expect (sum-of-lengths (list "a" "bc")) 3)

(define (sum-of-lengths los)
  (sum-of string-length los))

;; (X -> Number) (listof X) -> Number
;; Abstract function to apply the given function on every element of the given list
;; and return the sum of all the answer.

(define (sum-of func lox)
  (foldr + 0 (map func lox)))

(check-expect (sum-of sqr empty) 0)
(check-expect (sum-of sqr '(2 4)) 20)
(check-expect (sum-of string-length '("a" "bc")) 3)


;; PROBLEM B:
;;
;; Now re-define the original functions to use abstract-sum.
;;
;; Remember, the signature and tests should not change from the original
;; functions.
;; 
;; -> Redefined the functions in place.
