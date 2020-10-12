#lang htdp/bsl

; Data definitions:
;
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
; - self-reference: (rest los) is ListOfString


; Functions:
;
; PROBLEM:
;
; Design a function that consumes a list of strings and "yells" each word by
; adding "!" to the end of each string.

; ListOfString -> ListOfString
; Produces the list of string with ! appended to each element of the list

(define (yell los)
  (cond [(empty? los) empty]
        [else
         (cons (string-append (first los) "!")
              (yell (rest los)))]))

; Tests

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

(check-expect (yell LS0) empty)
(check-expect (yell LS1) (cons "a!" empty))
(check-expect (yell LS2) (cons "a!" (cons "b!" empty)))
(check-expect (yell LS3) (cons "c!" (cons "b!" (cons "a!" empty))))
