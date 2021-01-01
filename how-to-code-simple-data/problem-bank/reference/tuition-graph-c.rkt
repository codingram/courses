#lang htdp/bsl

(require 2htdp/image)

;; Remember the constants and data definitions we created in lectures 5h-j
;; to help Eva decide where to go to university:


;; Constants:

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE   1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")


;; Data definitions:

(define-struct school (name tuition))
;; School is (make-school String Natural)
;; interp. name is the school's name, tuition is international-students tuition in USD

#;
(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))

;; Template rules used:
;;  - compound: (make-school String Natural)


;; ListOfSchool is one of:
;;  - empty
;;  - (cons School ListOfSchool)
;; interp. a list of schools
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons School ListOfSchool)
;;  - reference: (first los) is School
;;  - self-reference: (rest los) is ListOfSchool


;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber


;; Functions:

#| PROBLEM A:
 |
 | Complete problem (C) from the reference rule videos.
 |
 | "Design a function that consumes information about schools and produces
 | the school with the lowest international student tuition."
 |
 | The function should consume a ListOfSchool. Call it cheapest.
 |
 | ; ASSUME the list includes at least one school or else
 | ;        the notion of cheapest doesn't make sense
 |
 | Also note that the template for a function that consumes a non-empty
 | list is:
 |
 | (define (fn-for-nelox nelox)
 |   (cond [(empty? (rest nelox)) (...  (first nelox))]
 |         [else
 |           (... (first nelox)
 |                (fn-for-nelox (rest nelox)))]))
 |
 | And the template for a function that consumes two schools is:
 |
 | (define (fn... s1 s2)
 | (... (school-name s1)
 |      (school-tuition s1)
 |      (school-name s2)
 |      (school-tuition s2))) |#

;; ListOfSchool -> School
;; Produces the school with the lowest tuition from the list of schools

(define (cheapest nelox)
  (cond [(empty? (rest nelox)) (first nelox)]
        [else
          (if (< (school-tuition (first nelox))
                 (school-tuition (cheapest (rest nelox))))
            (first nelox)
            (cheapest (rest nelox)))]))


;; Tests

(define S1 (make-school "School1" 27797))
(define S2 (make-school "School2" 23300))
(define S3 (make-school "School3" 28500))

(define LOS1 (cons S1 empty))
(define LOS2 (cons S2 (cons S1 empty)))
(define LOS3 (cons S1 (cons S3 (cons S2 empty))))

(check-expect (cheapest LOS1) S1)
(check-expect (cheapest LOS2) S2)
(check-expect (cheapest LOS3) S2)


#| PROBLEM B:
 |
 | Design a function that consumes a ListOfSchool and produces a list of the
 | school names. Call it get-names.
 |
 |  Do you need to define a new helper function? |#

;; ListOfName is one of:
;; - empty
;; - (cons String ListOfName)
;; interp. list of school names
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
          (... (first lon)
               (fn-for-lon (rest lon)))]))

;; ListOfSchool -> ListOfName
;; Produces the list of school names from the list of schools

(define (get-names lon)
  (cond [(empty? lon) empty]
        [else
          (cons (school-name (first lon))
                (get-names (rest lon)))]))


;; Tests

(check-expect (get-names empty) empty)
(check-expect (get-names LOS1) (cons "School1" empty))
(check-expect (get-names LOS2) (cons "School2" (cons "School1" empty)))
(check-expect (get-names LOS3) (cons "School1" (cons "School3" (cons "School2" empty))))
