#lang htdp/bsl+

;; Problem:
;;
;; Given the data definition below, design a function called zip that consumes two
;; lists of numbers and produces a list of Entry, formed from the corresponding
;; elements of the two lists.
;;
;; (zip (list 1 2 ...) (list 11 12 ...)) should produce:
;;
;; (list (make-entry 1 11) (make-entry 2 12) ...)
;;
;; Your design should assume that the two lists have the same length.


;; =================
;; Data Definitions:

(define-struct entry (key value))
;; Entry is (make-entry Number Number)
;; Interp. an entry maps a key to a value
(define E1 (make-entry 3 12))

;; ListOfEntry is one of:
;;  - empty
;;  - (cons Entry ListOfEntry)
;; interp. a list of key value entries
(define LOE1 (list E1 (make-entry 1 11)))


;; ==========
;; Functions:

;; ListOfNumber ListOfNumber -> ListOfEntry
;; Produces a list of Entry, formed from the corresponding elements of the two lists

(define (zip lista listb)
  (cond [(empty? lista) empty]
        [else
          (cons (make-entry (first lista) (first listb))
                (zip (rest lista) (rest listb)))]))


;; =======
;; Tests:

(check-expect (zip empty empty) empty)
(check-expect (zip '(1) '(2)) `(,(make-entry 1 2)))
(check-expect (zip '(1 2) '(3 4)) `(,(make-entry 1 3) ,(make-entry 2 4)))
(check-expect (zip '(1 2 3) '(1 2 3)) `(,(make-entry 1 1) ,(make-entry 2 2) ,(make-entry 3 3)))
