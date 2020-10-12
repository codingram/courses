#lang htdp/bsl

; HtDD Design Quiz

; Age is Natural
; interp. the age of a person in years
(define A0 18)
(define A1 25)

; Template rules used
; - atomic-non-distinct: Natural

#;
(define (fn-for-age a)
  (... a))


#| Problem 1:
 |
 | Consider the above data definition for the age of a person.
 |
 | Design a function called teenager? that determines whether a person
 | of a particular age is a teenager (i.e., between the ages of 13 and 19). |#


; Age -> Boolean)
; Determines whether a given Age falls between 13 and 19
(check-expect (teenager? 13) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 9) false)
(check-expect (teenager? 37) false)

; (define (teenager? age) false)  ;stub

(define (teenager? a)
  (and (>= a 13) (<= a 19)))



#| Problem 2:
 |
 | Design a data definition called MonthAge to represent a person's age
 | in months. |#


; MonthAge is Natural
; interp. represents a person's age in months
(define MA1 45)
(define MA2 145)
(define MA3 672)

; Template rules used:
; - atomic-non-distinct: Natural

#;
(define (fn-for-month-age ma)
  (... ma))



#| Problem 3:
 |
 | Design a function called months-old that takes a person's age in years
 | and yields that person's age in months. |#


; Age -> MonthAge
; Converts the age of a person from years to months
(check-expect (months-old 4) (* 12 4))
(check-expect (months-old 24) (* 12 24))
(check-expect (months-old 75) (* 12 75))
(check-expect (months-old 0) (* 12 0))

; (define (months-old a) 0)  ;stub

(define (months-old a)
  (* 12 a))



#| Problem 4:
 |
 | Consider a video game where you need to represent the health of your
 | character. The only thing that matters about their health is:
 |
 |   - if they are dead (which is shockingly poor health)
 |   - if they are alive then they can have 0 or more extra lives
 |
 | Design a data definition called Health to represent the health of your
 | character.
 |
 | Design a function called increase-health that allows you to increase the
 | lives of a character.  The function should only increase the lives
 | of the character if the character is not dead, otherwise the character
 | remains dead. |#


; Health is Natural[0, 100]
; interp. Natural represents the Health remaining
;         - 0 means they are dead
;         - 100 means full health
(define H1 0)
(define H2 14)
(define H3 100)

; Lives is Natural
; interp. Natural represents the Lives remaining
;         - 0 means no extra lives
;         - Any number greater than 0 represents the extra lives
(define L1 0)
(define L2 3)
