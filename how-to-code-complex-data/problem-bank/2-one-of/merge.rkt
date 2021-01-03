#lang htdp/bsl+


;; Problem:
;;
;; Design the function merge. It consumes two lists of numbers, which it assumes are
;; each sorted in ascending order. It produces a single list of all the numbers,
;; also sorted in ascending order.
;;
;; Your solution should explicitly show the cross product of type comments table,
;; filled in with the values in each case. Your final function should have a cond
;; with 3 cases. You can do this simplification using the cross product table by
;; recognizing that there are subtly equal answers.
;;
;; Hint: Think carefully about the values of both lists. You might see a way to
;; change a cell content so that 2 cells have the same value.


;; ListOfInteger ListOfInteger -> ListOfInteger
;; Merge the two given list in ascending order.
;; This is basically the merge part of the mergesort algorithm.

(define (merge lista listb)
  (cond [(empty? lista) listb]
        [(empty? listb) lista]
        [else
          (if (<= (first lista) (first listb))
            (cons (first lista) (merge (rest lista) listb))
            (cons (first listb) (merge lista (rest listb))))]))


;; ========
;; Tests:

(check-expect (merge empty empty) empty)
(check-expect (merge '(1 2) empty) '(1 2))
(check-expect (merge empty '(1 2)) '(1 2))
(check-expect (merge '(1) '(2)) '(1 2))
(check-expect (merge '(2) '(1)) '(1 2))
(check-expect (merge '(1 4) '(2)) '(1 2 4))
(check-expect (merge '(3 5) '(1)) '(1 3 5))
(check-expect (merge '(1 4) '(2 3 5)) '(1 2 3 4 5))
(check-expect (merge '(3 4 5) '(1 6)) '(1 3 4 5 6))
