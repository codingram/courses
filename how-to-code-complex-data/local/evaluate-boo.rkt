#lang htdp/isl


;; Given the following function definition:

(define (boo x lon)
  (local [(define (addx n) (+ n x))]
    (if (zero? x)
        empty
        (cons (addx (first lon))
              (boo (sub1 x) (rest lon))))))




;; PROBLEM A:
;;
;; What is the value of the following expression:
;;
;; (boo 2 (list 10 20))
;;
;; NOTE: We are not asking you to show the full step-by-step evaluation for
;; this problem, but you may want to sketch it out to help you get these
;; questions right.

(cons 12 (cons 21 empty))


;; PROBLEM B:
;;
;; How many function definitions are lifted during the evaluation of the
;; expression in part A.

3


;; PROBLEM C:
;;
;; Write out the lifted function definition(s). Just the actual lifted function
;; definitions.

; Is this a closure? The variable 'x' is not defined in the function scope when
; the function is being defined and so the value of 'x' from the outer scope is
; being closed and substitued (No idea about the actual implementation).
(define (addx_0 n) (+ n 2))
(define (addx_1 n) (+ n 1))
(define (addx_2 n) (+ n 0))
