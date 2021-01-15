#lang htdp/asl


;; PROBLEM:
;;
;; Design a function that consumes a list of numbers and produces true if the
;; numbers in lon are strictly decreasing. You may assume that the list has at
;; least two elements.


;; (listof Number) -> Boolean
;; Produces true if all the numbers in lon are strictly decreasing, false otherwise.

(define (strictly-decreasing? lon0)
  (local
    ; acc: Natural, represents the number before (first lon)
    ; (strictly-decreasing? '(9 5 2))
    ; (strictly-decreasing? '(  5 2) 9)
    ; (strictly-decreasing? '(    2) 5)
    ; (strictly-decreasing? '(     ) 2)
    [(define (helper lon acc)
       (if (empty? lon)
         true
         (if (< (first lon) acc)
           (helper (rest lon) (first lon))
           false)))]
    (helper (rest lon0) (first lon0))))


(check-expect (strictly-decreasing? '(-4 -6 -9)) true)
(check-expect (strictly-decreasing? '(2 1)) true)
(check-expect (strictly-decreasing? '(2 4 5)) false)
(check-expect (strictly-decreasing? '(1 1)) false)
(check-expect (strictly-decreasing? '(5 2)) true)
(check-expect (strictly-decreasing? '(9 5 2)) true)
