#lang htdp/asl


;; PROBLEM:
;;
;; Consider the following function that consumes Natural number n and produces the sum
;; of all the naturals in [0, n].
;;
;; Use an accumulator to design a tail-recursive version of sum-n.


;; Natural -> Natural
;; produce sum of Natural[0, n]

(check-expect (sum-n 0) 0)
(check-expect (sum-n 1) 1)
(check-expect (sum-n 3) (+ 3 2 1 0))

;(define (sum-n n) 0) ;0

(define (sum-n n0)
  (local
    ; acc: Number, represents the sum of numbers from [n0, n)
    ; (sum-n 3)
    ; (helper 3 0)
    ; (helper 2 3)
    ; (helper 1 5)
    ; (helper 0 6)
    [(define (helper n acc)
       (cond [(zero? n) acc]
             [else
               (helper (sub1 n)
                       (+ n acc))]))]
    (helper n0 0)))
