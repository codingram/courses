#lang htdp/isl


;; PROBLEM:
;;
;; Design an abstract function called some-pred? (including signature, purpose,
;; and tests) to simplify the following two functions. When you are done
;; rewrite the original functions to use your new some-pred? function.


;; ListOfNumber -> Boolean
;; produce true if some number in lon is positive
(check-expect (some-positive? empty) false)
(check-expect (some-positive? (list 2 -3 -4)) true)
(check-expect (some-positive? (list -2 -3 -4)) false)

(define (some-positive? lon)
  (some-pred? positive? lon))


;; ListOfNumber -> Boolean
;; produce true if some number in lon is negative
(check-expect (some-negative? empty) false)
(check-expect (some-negative? (list 2 3 -4)) true)
(check-expect (some-negative? (list 2 3 4)) false)

(define (some-negative? lon)
  (some-pred? negative? lon))


;; (X -> Boolean) (listof X) -> Boolean
;; Given a function 'func' and a list of elements 'lox', produce true if any of the element is true
;; for the given function, false otherwise
(define (some-pred? func lox)
  (cond [(empty? lox) false]
        [else
          (or (func (first lox))
              (some-pred? func (rest lox)))]))


(check-expect (some-pred? positive? empty) false)
(check-expect (some-pred? positive? '(2 3 -3)) true)
(check-expect (some-pred? negative? '(2 3)) false)
