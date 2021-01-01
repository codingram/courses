#lang htdp/bsl

;; Data definitions:
;;
;; Remember the data definition for a list of numbers we designed in Lecture 5f:
;; (if this data definition does not look familiar, please review the lecture)


;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber


;; Functions:
;;
;; PROBLEM:
;;
;; Design a function that consumes a list of numbers and produces the largest number
;; in the list. You may assume that all numbers in the list are greater than 0. If
;; the list is empty, produce 0.

(define (largest-number lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon) (largest-number (rest lon)))
           (first lon)
           (largest-number (rest lon)))]))


;; Tests

(define LON1 empty)
(define LON2 (cons 45 empty))
(define LON3 (cons 60 (cons 42 empty)))
(define LON4 (cons 34 (cons 35 (cons 100 empty))))
(define LON5 (cons 23 (cons 56 (cons 56 empty))))

(check-expect (largest-number LON1) 0)
(check-expect (largest-number LON2) 45)
(check-expect (largest-number LON3) 60)
(check-expect (largest-number LON4) 100)
(check-expect (largest-number LON5) 56)
