#lang htdp/isl


;; I wanted to try out lambda which is an alternate way on defining function.
;; The syntax is: (define name (lambda (variable variable ...) expression))
;; A lambda cannot be used outside of this alternate syntax.
(define circle-area (lambda (radius) (* pi (sqr radius))))


(circle-area 4) ; (* pi (sqr 4)) ;area of circle radius 4
(circle-area 6) ; (* pi (sqr 6)) ;area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"
(check-expect (contains-ubc? empty) false)
(check-expect (contains-ubc? (cons "McGill" empty)) false)
(check-expect (contains-ubc? (cons "UBC" empty)) true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) true)

;<template from ListOfString>
(define (contains-ubc? los) (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
(check-expect (contains-mcgill? empty) false)
(check-expect (contains-mcgill? (cons "UBC" empty)) false)
(check-expect (contains-mcgill? (cons "McGill" empty)) true)
(check-expect (contains-mcgill? (cons "UBC" (cons "McGill" empty))) true)

;<template from ListOfString>
(define (contains-mcgill? los) (contains? "McGill" los))


;; String (listof String) -> Boolean
;; Produce true if str is in los (list of string), false otherwise

(define (contains? str los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) str)
             true
             (contains? str (rest los)))]))


;; Tests:

(check-expect (contains? "UBC" empty) false)
(check-expect (contains? "UBC" '("McGill")) false)
(check-expect (contains? "UBC" '("UBC")) true)
(check-expect (contains? "McGill" '("McGill" "UBC")) true)
(check-expect (contains? "McGill" '("UBC" "McGill")) true)
(check-expect (contains? "Random" '("UBC" "McGill")) false)


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

;<template from ListOfNumber>
(define (squares lon) (map2 sqr lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

;<template from ListOfNumber>
(define (square-roots lon) (map2 sqrt lon))


;; Type parameters: The first argument to map2 is a function and the second argument is a list.
;; Now, we don't know what type of elements are present in the list but as we are going to pass
;; them in the given function, they can be denoted using type parameters. The type of the type
;; parameters is Any by default. What this signature says is whatever the type of elements is in
;; the given list, the same type is for the given function parameter and whatever type of element
;; the given function returns, the same type of elements are present in the return type for the
;; map2 function.

;; (X -> Y) (listof X) -> (listof Y)
;; Given a function 'func' and a list of numbers 'lon', produce a list of elements where each
;; element is the return value when the function 'func' is applied on every element in 'lon'

(define (map2 func lon)
  (cond [(empty? lon) empty]
        [else
         (cons (func (first lon))
               (map2 func (rest lon)))]))


;; Tests:

(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr '(2 3)) '(4 9))
(check-expect (map2 sqrt '(16 25)) '(4 5))
(check-expect (map2 abs '(2 -3)) '(2 3))
(check-expect (map2 string-length '("a" "ab" "abcd")) '(1 2 4))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

;<template from ListOfNumber>
(define (positive-only lon) (filter2 positive? lon))

;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

;<template from ListOfNumber>
(define (negative-only lon) (filter2 negative? lon))


;; (X -> Boolean) (listof X) -> (listof X)
;; Given a function 'func' and a list of numbers 'lon', removes all the elements in the given
;; list 'lon' whose return value when passed in the function 'func' is false and produces the
;; list of remaining elements.

(define (filter2 func? lon)
  (cond [(empty? lon) empty]
        [else
         (if (func? (first lon))
             (cons (first lon)
                   (filter2 func? (rest lon)))
             (filter2 func? (rest lon)))]))


;; Tests

(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 positive? '(1 2)) '(1 2))
(check-expect (filter2 positive? '(1 -2 -3)) '(1))
(check-expect (filter2 negative? '(2 -3 -1)) '(-3 -1))
