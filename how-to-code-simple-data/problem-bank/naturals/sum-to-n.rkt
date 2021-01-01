#lang htdp/bsl

#| PROBLEM:

Design a function that produces the sum of all the naturals from 0 to a given n.  |#


;; Natural -> Natural
;; Produces the sum of all Natural from 0 to n

(define (sum n)
  (cond [(zero? n) 0]
        [else
          (+ n (sum (sub1 n)))]))


;; Tests

(check-expect (sum 0) 0)
(check-expect (sum 1) 1)
(check-expect (sum 4) 10)
(check-expect (sum 10) 55)
