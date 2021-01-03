#lang htdp/bsl+

;; Problem:
;;
;; Given the data definition below, design a function called concat that
;; consumes two ListOfString and produces a single list with all the elements
;; of lsta preceding lstb.
;;
;; (concat (list "a" "b" ...) (list "x" "y" ...)) should produce:
;;
;; (list "a" "b" ... "x" "y" ...)
;;
;; You are basically going to design the function append using a cross product
;; of type comments table. Be sure to simplify your design as much as possible.
;;
;; Hint: Think carefully about the values of both lists. You might see a way to
;; change a cell's content so that 2 cells have the same value.


;; =================
;; Data Definitions:

;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))


;; ==========
;; Functions:

;; ListOfString ListOfString -> ListOfString
;; Concatenates the two list of string to produce one list of string

(define (concat lista listb)
  (cond [(empty? lista) listb]
        [(empty? listb) lista]
        [else
          (cons (first lista)
                (concat (rest lista) listb))]))


;; =======
;; Tests:

(check-expect (concat empty empty) empty)
(check-expect (concat '("a" "b") empty) '("a" "b"))
(check-expect (concat empty '("a" "b")) '("a" "b"))
(check-expect (concat '("a") '("b")) '("a" "b"))
(check-expect (concat '("a" "b") '("c" "d")) '("a" "b" "c" "d"))
(check-expect (concat '("a") '("d" "c")) '("a" "d" "c"))
(check-expect (concat '("b" "c") '("d")) '("b" "c" "d"))
