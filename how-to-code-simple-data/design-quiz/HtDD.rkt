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

; (define (teenager? age) false)  ;stub

(define (teenager? a)
  (and (>= a 13) (<= a 19)))

; Tests

(check-expect (teenager? 13) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 9) false)
(check-expect (teenager? 37) false)


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

; (define (months-old a) 0)  ;stub

(define (months-old a)
  (* 12 a))

; Tests

(check-expect (months-old 4) (* 12 4))
(check-expect (months-old 24) (* 12 24))
(check-expect (months-old 75) (* 12 75))
(check-expect (months-old 0) (* 12 0))


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

; Data definitions

; Health is one of:
; - false
; - Natural
; interp. Natural represents the Health remaining
;         - false means they are dead
;         - Natural represents their extra lives
(define H1 false)
(define H2 1)
(define H3 12)
#;
(define (fn-for-health h)
  (cond [(false? h) (... h)]
        [else (... h)]))


; Function definitions

; Health Natural -> Health
; Add n additional lives to non-dead character

(define (increase-health h n)
  (cond [(false? h) false]
        [else (+ h n)]))


; Tests

(check-expect (increase-health false 3) false)
(check-expect (increase-health 0 1) 1)
(check-expect (increase-health 3 2) 5)
