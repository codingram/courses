#lang htdp/bsl

; Data definitions:
;
; PROBLEM A:
;
; Design a data definition to represent a list of booleans. Call it ListOfBoolean.

; ListOfBoolean is one of:
; - empty
; - (cons Boolean ListOfBoolean)
; interp. list of Boolean values

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

; Functions:
;
; PROBLEM B:
;
; Design a function that consumes a list of boolean values and produces true
; if every value in the list is true. If the list is empty, your function
; should also produce true. Call it all-true?

; ListOfBooleans -> Boolean
; Produces true if all the values in the list is true else false

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))


; Tests

(define LOB1 empty)
(define LOB2 (cons true empty))
(define LOB3 (cons false empty))
(define LOB4 (cons true (cons false empty)))
(define LOB5 (cons false (cons true empty)))
(define LOB6 (cons true (cons true empty)))
(define LOB7 (cons true (cons true (cons true empty))))
(define LOB8 (cons true (cons true (cons false empty))))

(check-expect (all-true? LOB1) true)
(check-expect (all-true? LOB2) true)
(check-expect (all-true? LOB3) false)
(check-expect (all-true? LOB4) false)
(check-expect (all-true? LOB5) false)
(check-expect (all-true? LOB6) true)
(check-expect (all-true? LOB7) true)
(check-expect (all-true? LOB8) false)
