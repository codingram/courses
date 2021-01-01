#lang htdp/bsl+

;; Problem:
;;
;; It is often useful to be able to tell whether the first part of a sequence of
;; characters matches a given pattern. In this problem you will design a (somewhat
;; limited) way of doing this.
;;
;; Assume the following type comments and examples:


;; =================
;; Data Definitions:

;; OneString is String
;; interp. these are strings only one character long
(define EXAMPLE-ONE-SA "x")
(define EXAMPLE-ONE-SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOfOneString.
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define EXAMPLE-PATTERN (list "A" "N" "A" "N" "A" "N"))

;; ListOfOneString is one of:
;;  - empty
;;  - (cons OneString ListOfOneString)
;; interp. a list of strings each one long
(define EXAMPLE-LOS (list "V" "6" "T" "1" "Z" "4"))


;; Now design a function that consumes Pattern and ListOfOneString and produces true
;; if the pattern matches the ListOfOneString. For example,
;;
;; (pattern-match? (list "A" "N" "A" "N" "A" "N")
;;                 (list "V" "6" "T" "1" "Z" "4"))
;;
;; should produce true. If the ListOfOneString is longer than the pattern, but the
;; first part matches the whole pattern produce true. If the ListOfOneString is
;; shorter than the Pattern you should produce false.
;;
;; Treat this as the design of a function operating on 2 complex data. After your
;; signature and purpose, you should write out a cross product of type comments
;; table. You should reduce the number of cases in your cond to 4 using the table,
;; and you should also simplify the cond questions using the table.
;;
;; You should use the following helper functions in your solution:


;; ==========
;; Functions:

;; OneString -> Boolean
;; Produces true if one-string is alphabetic/numeric, false otherwise

(define (alphabetic? one-string) (char-alphabetic? (string-ref one-string 0)))
(define (numeric? one-string) (char-numeric? (string-ref one-string 0)))


;; Pattern ListOfOneString -> Boolean
;; Produces true if the given pattern matches the given list of one string, false
;; otherwise.

(define (pattern-match? pattern list-of-one-string)
  (cond [(empty? pattern) true]
        [(empty? list-of-one-string) false]
        [(string=? (first pattern) "A")
         (and (alphabetic? (first list-of-one-string))
              (pattern-match? (rest pattern) (rest list-of-one-string)))]
        [(string=? (first pattern) "N")
         (and (numeric? (first list-of-one-string))
              (pattern-match? (rest pattern) (rest list-of-one-string)))]))


;; ==========
;; Tests:

(define P0 empty)
(define P1 '("A"))
(define P2 '("N"))
(define P3 '("A" "A" "A"))
(define P4 '("N" "N" "N"))
(define P5 '("A" "N" "N" "A"))
(define P6 '("N" "A" "N" "N"))

(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(check-expect (pattern-match? P0 empty) true)
(check-expect (pattern-match? P0 '("A" "2")) true)
(check-expect (pattern-match? P5 empty) false)
(check-expect (pattern-match? P6 empty) false)
(check-expect (pattern-match? P1 '("G")) true)
(check-expect (pattern-match? P1 '("3")) false)
(check-expect (pattern-match? P2 '("6")) true)
(check-expect (pattern-match? P2 '("B")) false)
(check-expect (pattern-match? P3 '("G" "D" "E")) true)
(check-expect (pattern-match? P3 '("B" "3" "D")) false)
(check-expect (pattern-match? P4 '("2" "3" "4")) true)
(check-expect (pattern-match? P4 '("4" "2" "G")) false)
(check-expect (pattern-match? P5 '("F" "2" "5" "A")) true)
(check-expect (pattern-match? P5 '("A" "2" "G" "A")) false)
(check-expect (pattern-match? P6 '("2" "A" "2" "3")) true)
(check-expect (pattern-match? P6 '("3" "C" "A" "3")) false)
