#lang htdp/bsl

; Data definitions:

; Remember the data definition for a list of strings we designed in Lecture 5c:
; (if this data definition does not look familiar, please review the lecture)

; ListOfString is one of:
;  - empty
;  - (cons String ListOfString)
; interp. a list of strings


#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

; Template rules used:
; - one of: 2 cases
; - atomic distinct: empty
; - compound: (cons String ListOfString)
; - atomic non-distinct: (first los) is  String
; - self-reference: (rest los) is ListOfString


; Functions:

; PROBLEM:
;
; Design a function that consumes a list of strings and determines the total
; number of characters (single letters) in the list. Call it total-string-length.

; ListOfString -> Natural
; Returns the total number of characters in the list

(define (total-string-length los)
  (cond [(empty? los) 0]
        [else
         (+ 1
            (total-string-length (rest los)))]))

; Tests

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

(check-expect (total-string-length LS0) 0)
(check-expect (total-string-length LS1) 1)
(check-expect (total-string-length LS2) 2)
(check-expect (total-string-length LS3) 3)
