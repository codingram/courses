#lang htdp/bsl

;; Data definitions:

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

;; PROBLEM:
;;
;; Design a function that consumes a list of numbers and doubles every number
;; in the list. Call it double-all.

;; ListOfNumber -> ListOfNumber
;; Returns the list with every number doubled

(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* (first lon) 2)
              (double-all (rest lon)))]))


;; Tests

(define LON1 empty)
(define LON2 (cons 10 empty))
(define LON3 (cons 60 (cons 42 empty)))
(define LON4 (cons 35 (cons 50 (cons 100 empty))))

(check-expect (double-all LON1) empty)
(check-expect (double-all LON2) (cons 20 empty))
(check-expect (double-all LON3) (cons 120 (cons 84 empty)))
(check-expect (double-all LON4) (cons 70 (cons 100 (cons 200 empty))))
