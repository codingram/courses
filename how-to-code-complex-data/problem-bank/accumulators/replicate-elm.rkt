#lang htdp/asl


;; PROBLEM:
;;
;; Design a function that consumes a list of elements and a natural n, and produces
;; a list where each element is replicated n times.
;;
;; (replicate-elm (list "a" "b" "c") 2) should produce (list "a" "a" "b" "b" "c" "c")


;; (listof X) Natural -> (listof X)
;; Produces a list where each element is replicated n times.

(define (replicate-elm lox0 n)
  (local
    ; acc:
    ; (replicate-elm '(1 2) 2)
    ;
    ; (helper '(1 2) 1)
    ; (helper '(1 2) 2)
    ; (helper '(  2) 1)
    ; (helper '(  2) 2)
    ; (helper '(   ) 1)
    [(define (helper lox acc)
       (if (empty? lox)
         empty
         (if (not (= n acc))
           (cons (first lox) (helper lox (add1 acc)))
           (helper (rest lox) 0))))]
    (helper lox0 0)))


(check-expect (replicate-elm empty 2) empty)
(check-expect (replicate-elm '(1) 2) '(1 1))
(check-expect (replicate-elm '(1 2) 1) '(1 2))
(check-expect (replicate-elm '(1 2) 3) '(1 1 1 2 2 2))
