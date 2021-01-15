#lang htdp/asl

;; PROBLEM:
;;
;; Previously we have written functions to count the number of elements in a list. In this
;; problem we want a function that produces separate counts of the number of odd and even
;; numbers in a list, and we only want to traverse the list once to produce that result.
;;
;; Design a tail recursive function that produces the Counts for a given list of numbers.
;; Your function should produce Counts, as defined by the data definition below.
;;
;; There are two ways to code this function, one with 2 accumulators and one with a single
;; accumulator. You should provide both solutions.


(define-struct counts (odds evens))
;; Counts is (make-counts Natural Natural)
;; interp. describes the number of even and odd numbers in a list

(define C1 (make-counts 0 0)) ;describes an empty list
(define C2 (make-counts 3 2)) ;describes (list 1 2 3 4 5))


;; (listof Natural) -> Counts
;; Produces the count instance which represents the number of odd and even numbers
;; in the given list.

;; Another way would be to directly use the structure
(define (odd-even-count1 lon0)
  (local
    ; odd: Natural, represents the total odd number count
    ; (odd-even-count '(1 2 3 4 5))
    ; (helper '(1 2 3 4 5) 0)
    ; (helper '(  2 3 4 5) 1)
    ; (helper '(    3 4 5) 1)
    ; (helper '(      4 5) 2)
    ; (helper '(        5) 2)
    ; (helper '(         ) 3)
    [(define (helper lon odd)
       (if (empty? lon)
         (make-counts odd (- (length lon0) odd))
         (if (odd? (first lon))
           (helper (rest lon) (add1 odd))
           (helper (rest lon) odd))))]
    (helper lon0 0)))

(define (odd-even-count2 lon0)
  (local
    ; odd: Natural, represents the total odd number count
    ; even: Natural, represents the total even number count
    ; (odd-even-count '(1 2 3 4 5))
    ; (helper '(1 2 3 4 5) 0 0)
    ; (helper '(  2 3 4 5) 1 0)
    ; (helper '(    3 4 5) 1 1)
    ; (helper '(      4 5) 2 1)
    ; (helper '(        5) 2 2)
    ; (helper '(         ) 3 2)
    [(define (helper lon odd even)
       (if (empty? lon)
         (make-counts odd even)
         (if (odd? (first lon))
           (helper (rest lon) (add1 odd) even)
           (helper (rest lon) odd (add1 even)))))]
    (helper lon0 0 0)))


(check-expect (odd-even-count1 empty) C1)
(check-expect (odd-even-count1 '(1 2 3 4 5)) C2)
(check-expect (odd-even-count1 '(1 3 5)) (make-counts 3 0))
(check-expect (odd-even-count1 '(0 2 4)) (make-counts 0 3))

(check-expect (odd-even-count2 empty) C1)
(check-expect (odd-even-count2 '(1 2 3 4 5)) C2)
(check-expect (odd-even-count2 '(1 3 5)) (make-counts 3 0))
(check-expect (odd-even-count2 '(0 2 4)) (make-counts 0 3))
