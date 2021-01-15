#lang htdp/asl


;; PROBLEM:
;;
;; Consider the following function that consumes a list of numbers and produces the sum of
;; all the odd numbers in the list.
;;
;; Use an accumulator to design a tail-recursive version of sum-odds.


;; (listof Number) -> Number
;; produce sum of all odd numbers of lon
(check-expect (sum-odds empty) 0)
(check-expect (sum-odds (list 1 2 5 6 11)) 17)

(define (sum-odds lon0)
  (local
    ; acc: Number, represents the sum of odd numbers so far.
    ; (sum-odds '(1 2 5 6))
    ; (helper '(1 2 5 6) 0)
    ; (helper '(  2 5 6) 1)
    ; (helper '(    5 6) 1)
    ; (helper '(      6) 6)
    ; (helper '(       ) 6)
    [(define (helper lon acc)
       (cond [(empty? lon) acc]
             [else
               (if (odd? (first lon))
                 (helper (rest lon) (+ (first lon) acc))
                 (helper (rest lon) acc))]))]
    (helper lon0 0)))
