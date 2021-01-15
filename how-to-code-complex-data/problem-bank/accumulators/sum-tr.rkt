#lang htdp/asl

;; PROBLEM:
;;
;; (A) Consider the following function that consumes a list of numbers and produces
;;     the sum of all the numbers in the list. Use the stepper to analyze the behavior
;;     of this function as the list gets larger and larger.
;;
;; (B) Use an accumulator to design a tail-recursive version of sum.


;; (listof Number) -> Number
;; produce sum of all elements of lon
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)

#;
(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))


(define (sum lon0)
  (local
    ; acc: Number, the sum of the numbers so far
    ; (sum '(1 2 3 4 5))
    ;
    ; (sum '(1 2 3 4 5) 0)
    ; (sum '(  2 3 4 5) 1)
    ; (sum '(    3 4 5) 3)
    ; (sum '(      4 5) 6)
    ; (sum '(        5) 10)
    ; (sum '(         ) 15)
    [(define (helper lon acc)
       (if (empty? lon)
         acc
         (helper (rest lon)
                 (+ acc (first lon)))))]
    (helper lon0 0)))

;; foldr is an abstract recursive function while foldl is a tail recursive abstract function.
