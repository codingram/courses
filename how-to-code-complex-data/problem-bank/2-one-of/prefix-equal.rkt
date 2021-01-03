#lang htdp/bsl+


;; PROBLEM: design a function that consumes two lists of strings and produces true
;; if the first list is a prefix of the second. Prefix means that the elements of
;; the first list match the elements of the second list 1 for 1, and the second list
;; is at least as long as the first.
;;
;; For reference, the ListOfString data definition is provided below.

;; =================
;; Data Definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; ==========
;; Functions:

;; ListOfString ListOfString -> Boolean
;; Produce true if first list is a prefix of second list

(define (prefix=? lista listb)
  (cond [(empty? lista) true]
        [(empty? listb) false]
        [else (and (string=? (first lista) (first listb))
                   (prefix=? (rest lista) (rest listb)))]))


;; ========
;; Tests:

(check-expect (prefix=? empty empty) true)
(check-expect (prefix=? empty '("a" "b")) true)
(check-expect (prefix=? '("a" "b") empty) false)
(check-expect (prefix=? '("a") '("a")) true)
(check-expect (prefix=? '("a") '("b")) false)
(check-expect (prefix=? '("a" "b") '("a" "c")) false)
(check-expect (prefix=? '("a" "b") '("a" "b")) true)
(check-expect (prefix=? '("a" "b") '("a" "b" "c")) true)
(check-expect (prefix=? '("a" "b" "c") '("a" "b")) false)
