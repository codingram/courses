#lang htdp/asl


;; PROBLEM:
;;
;; Consider the following function that consumes Natural number n and produces a list of all
;; the naturals of the form (list 1 2 ... n-1 n) not including 0.
;;
;; Use an accumulator to design a tail-recursive version of to-list.


;; Natural -> (listof Natural)
;; produce (cons n (cons n-1 ... empty)), not including 0
(check-expect (to-list 0) empty)
(check-expect (to-list 1) (list 1))
(check-expect (to-list 3) (list 1 2 3))

;(define (to-list n) empty) ;stub

(define (to-list n)
  (local
    ; acc: (listof Natural) ; represents list of number seen so far.
    ; (to-list 3)
    ; (helper 3 '())
    ; (helper 2 '(3))
    ; (helper 1 '(2 3))
    ; (helper 0 '(1 2 3))
    [(define (helper n acc)
       (cond [(zero? n) acc]
             [else
               (helper (sub1 n)
                       (cons n acc))]))]
    (helper n '())))
