#lang htdp/asl


;; PROBLEM:
;;
;; Design a function that consumes (listof X) and produces a list of the same
;; elements in the opposite order. Use an accumulator. Call the function rev.
;; (DrRacket's version is called reverse.) Your function should be tail recursive.
;;
;; In this problem only the first step of templating is provided.

;; (listof X) -> (listof X)
;; produce list with elements of lox in reverse order
(check-expect (rev empty) empty)
(check-expect (rev (list 1)) (list 1))
(check-expect (rev (list "a" "b" "c")) (list "c" "b" "a"))
(check-expect (rev (list 1 2 3)) (list 3 2 1))

;(define (rev lox) empty)

(define (rev lox0)
  (local
    ; acc: (listof X) ; represents list of elements seen so far in reverse order.
    ; (rev '(1 2 3))
    ; (helper '(1 2 3) '())
    ; (helper '(  2 3) '(1))
    ; (helper '(    3) '(2 1))
    ; (helper '(     ) '(3 2 1))
    [(define (helper lox acc)
       (cond [(empty? lox) acc]
             [else
               (helper (rest lox)
                       (cons (first lox) acc))]))]
    (helper lox0 '())))
