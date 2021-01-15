#lang htdp/asl


;; PROBLEM:
;;
;; Design a function called average that consumes (listof Number) and produces the
;; average of the numbers in the list.


;; (listof Number) -> Number
;; Produces the average of the numbers in the list

(define (average lon0)
  (local
    ; sum: Number, represents the sum of numbers in the list so far.
    ; qty: Natural, represents the quantity of numbers in the list so far.
    ; (average '(1 3 5))
    ; (helper '(1 3 5) 0 0)
    ; (helper '(  3 5) 1 1)
    ; (helper '(    5) 4 2)
    ; (helper '(     ) 9 3)
    [(define (helper lon sum qty)
       (if (empty? lon)
         (if (zero? qty)
           0
           (/ sum qty))
         (helper (rest lon)
                 (+ (first lon) sum)
                 (add1 qty))))]
    (helper lon0 0 0)))


(check-expect (average empty) 0)
(check-expect (average '(2)) 2)
(check-expect (average '(1 3 5)) 3)
