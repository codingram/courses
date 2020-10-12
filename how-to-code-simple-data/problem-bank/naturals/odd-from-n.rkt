#lang htdp/bsl

#| PROBLEM:

Design a function called odd-from-n that consumes a natural number n,
and produces a list of all the odd numbers from n down to 1.

Note that there is a primitive function, odd?, that produces true
if a natural number is odd. |#


; Natural -> ListOfNatural
; Produces the list of odd natural number from n up to and including 1

(define (odd-from-n n)
  (cond [(zero? n) empty]
        [else
          (if (odd? n)
            (cons n (odd-from-n (sub1 n)))
            (odd-from-n (sub1 n)))]))


; Tests

(check-expect (odd-from-n 0) empty)
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 2) (cons 1 empty))
(check-expect (odd-from-n 3) (cons 3 (cons 1 empty)))
(check-expect (odd-from-n 6) (cons 5 (cons 3 (cons 1 empty))))
