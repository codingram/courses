#lang htdp/asl

;; PROBLEM 1:
;;
;; Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;; and produces the length of the longest string in the list.

;; (listof String) -> Natural
;; Produces the length of the longest string in the given list

(define (longest-string los)
  ; longest is Natural; represents the longest string seen so far
  (local
    [(define (helper los longest)
       (cond [(empty? los) longest]
             [else
               (let ([len (string-length (first los))])
                 (if (> len longest)
                   (helper (rest los) len)
                   (helper (rest los) longest)))]))]
    (helper los 0)))

#; ; Alternate version using foldr
(define (longest-string los)
  (foldr
    (lambda (str longest)
      (let ([len (string-length str)])
        (if (> len longest)
          len
          longest)))
    0
    los))

(check-expect (longest-string empty) 0)
(check-expect (longest-string '("" "")) 0)
(check-expect (longest-string '("abc" "ab")) 3)
(check-expect (longest-string '("ab" "abcd")) 4)
(check-expect (longest-string '("ab" "asb" "abshd")) 5)



;; PROBLEM 2:
;;
;; The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is
;; the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to
;; n-2 + n-1.
;;
;; Design a function that given a list of numbers at least two elements long,
;; determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every
;; element in the list. The sequence does not have to start at zero, so for
;; example, the sequence 4, 5, 9, 14, 23 would follow the rule.


;; (listof Integer) -> Boolean
;; Determines if the given list obeys the finonacci rule
;; Assumption: List is atleast 2 elements long

(define (fibo? lst)
  (cond [(= (length lst) 2) true]
        [else
          (if (= (+ (first lst) (second lst)) (third lst))
            (fibo? (rest lst))
            false)]))


(check-expect (fibo? '(1 1)) true)
(check-expect (fibo? '(5 10)) true)
(check-expect (fibo? '(1 1 2 3 5)) true)
(check-expect (fibo? '(1 1 2 4 5)) false)
(check-expect (fibo? '(1 2 5 7 12)) false)
(check-expect (fibo? '(4 5 9 14 23)) true)


;; PROBLEM 3:
;;
;; Refactor the function below to make it tail recursive.


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)


(define (fact n)
  ; res is Natural; represents a result so far accumulator
  (local
    [(define (helper n res)
       (cond [(zero? n) res]
             [else
               (helper (sub1 n) (* res n))]))]
    (helper n 1)))



;; PROBLEM 4:
;;
;; Recall the data definition for Region from the Abstraction Quiz. Use a worklist
;; accumulator to design a tail recursive function that counts the number of regions
;; within and including a given region.
;; So (count-regions CANADA) should produce 7


(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))

          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))


;; Region -> Natural
;; Counts the number of region, including the given region

(define (count-regions r)
  ; rem is (listof Region); represents a worklist accumulator
  ; total is Natural; represents the total number of region seen so far
  (local [(define (fn-for-region rem r total)
            (fn-for-lor
              (append (region-subregions r) rem)
              (add1 total)))

          (define (fn-for-lor lor total)
            (cond [(empty? lor) total]
                  [else
                    (fn-for-region (rest lor) (first lor) total)]))]
    (fn-for-region empty r 0)))


(check-expect (count-regions VICTORIA) 1)
(check-expect (count-regions CANADA) 7)
