#lang htdp/isl

;; Given the following partial data definitions:

(define-struct bag (l w h))
;; Bag is (make-bag Number Number Number)
;; interp. a bag with a length, width and height in centimeters
(define B1 (make-bag 19.5 10.0 6.5))
(define B2 (make-bag 23.0 11.5 7.0))
(define B3 (make-bag 18.0 9.5 5.5))

;; ListOfBag is one of:
;; - empty
;; - (cons Bag ListOfBag)
;; interp. a list of bags
(define LOB1 empty)
(define LOB2 (list B1 B2 B3))


;; PROBLEM:
;;
;; The linear length of a bag is defined to be it length plus
;; width plus height. Design the function linear-length-lob that consumes
;; a list of bags and produces a list of the linear lengths of each of
;; the bags in the list.
;;
;; Use at least one built-in abstract function and encapsulate any helper
;; functions in a local expression.


;; (listof Bag) -> (listof Number)
;; Produces the list of linear length for all the provided bags

(define (linear-length-lob lob)
  (local
    [(define (linear-length bag)
       (+ (bag-l bag) (bag-w bag) (bag-h bag)))]
    (map linear-length lob)))


(check-expect (linear-length-lob empty) empty)
(check-expect (linear-length-lob LOB2) '(36.0 41.5 33.0))
