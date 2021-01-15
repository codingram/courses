#lang htdp/asl


;; PROBLEM:
;;
;; Design a function that consumes a list of elements lox and a natural number
;; n and produces the list formed by including the first element of lox, then
;; skipping the next n elements, including an element, skipping the next n
;; and so on.
;;
;;  (skipn (list "a" "b" "c" "d" "e" "f") 2) should produce (list "a" "d")


;; (listof X) Natural -> (listof X)
;; produce list consisting of the elements skipping in between n elements
(check-expect (skipn empty 3) empty)
(check-expect (skipn '("a" "b" "c" "d" "e" "f") 2) '("a" "d"))
(check-expect (skipn '(1 2 3) 0) '(1 2 3))


(define (skipn lox0 n)
  (local
    ;; acc: Natural; number of elements skipped in lox
    ;; If the number of elements equals n, include (first lox) and reset the count to 0
    ;; otherwise add one to the accumulator
    ;; (skipn '("a" "b" "c" "d") 2) --> outer call
    ;;
    ;; (skipn '("a" "b" "c" "d") 2)
    ;; (skipn '(    "b" "c" "d") 0)
    ;; (skipn '(        "c" "d") 1)
    ;; (skipn '(            "d") 2)
    ;; (skipn '(               ) 0)
    [(define (helper lox acc)
       (if (empty? lox)
         empty
         (if (= acc n)
           (cons (first lox)
                 (helper (rest lox) 0))
           (helper (rest lox) (add1 acc)))))]
    (helper lox0 n)))


;; Alternate solutions:
;; - Instead of counting how many elements have been skipped, do a countdown from n to 0,
;;   so that when we get to 0 we include the (first lox) element.
;; - Use (remainder index (add1 n)) where index is zero-based.
