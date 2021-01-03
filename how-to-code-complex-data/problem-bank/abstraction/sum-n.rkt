#lang htdp/isl


;; PROBLEM:
;;
;; Complete the design of the following function, by writing out
;; the final function definition. Use at least one built in abstract
;; function.


;; Natural -> Natural
;; produce the sum of the first n odd numbers
(check-expect (sum-n-odds 0) 0)
(check-expect (sum-n-odds 1) (+ 0 1))
(check-expect (sum-n-odds 3) (+ 0 1 3 5))

;(define (sum-n-odds n) 0)  ;stub

(define (sum-n-odds n)
  (local
    [(define (next-odd n)
       (+ (* 2 n) 1))]
    (foldr + 0 (build-list n next-odd))))

;; One-liner
#;
(define (sum-n-odds n)
  (foldr + 0 (filter odd? (build-list (* 2 n) identity))))


;; HINT: The first n odd numbers are contained in the first 2*n naturals.
;; For example (list 0 1 2 3) contains the first 4 naturals and the first
;; 2 odd numbers. Also remember that DrRacket has a build in predicate
;; function called odd?
