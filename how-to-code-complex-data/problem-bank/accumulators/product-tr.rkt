#lang htdp/asl


;; PROBLEM:
;;
;; (A) Consider the following function that consumes a list of numbers and produces
;;     the product of all the numbers in the list. Use the stepper to analyze the behavior
;;     of this function as the list gets larger and larger.
;;
;; (B) Use an accumulator to design a tail-recursive version of product.


;; (listof Number) -> Number
;; produce product of all elements of lon
(check-expect (product empty) 1)
(check-expect (product (list 2 3 4)) 24)

(define (product lon)
  (local
    ; acc: Number, represents the result of product of the numbers so far.
    ; (product '(2 3 4))
    ; (helper '(2 3 4) 1)
    ; (helper '(  3 4) 2)
    ; (helper '(    4) 6)
    ; (helper '(     ) 24)
    [(define (helper lon acc)
       (cond [(empty? lon) acc]
             [else
               (helper (rest lon)
                       (* (first lon) acc))]))]
    (helper lon 1)))
