#lang htdp/bsl

#| Consider the following alternative type comment for Eva's school tuition
 | information program. Note that this is just a single type, with no reference,
 | but it captures all the same information as the two types solution in the
 | videos.
 |
 | (define-struct school (name tuition next))
 | ;; School is one of:
 | ;;  - false
 | ;;  - (make-school String Natural School)
 | ;; interp. an arbitrary number of schools, where for each school we have its
 | ;;         name and its tuition in USD
 |
 | (A) Confirm for yourself that this is a well-formed self-referential data
 |     definition.
 | - It is because it has a base case and a self reference case
 |
 | (B) Complete the data definition making sure to define all the same examples as
 |     for ListOfSchool in the videos.
 |
 | (C) Design the chart function that consumes School. Save yourself time by
 |     simply copying the tests over from the original version of chart.
 |
 | (D) Compare the two versions of chart. Which do you prefer? Why? |#

(require 2htdp/image)

(define-struct school (name tuition next))
;; School is one of:
;; - false
;; - (make-school (String Natural School))
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in INR

(define (fn-for-school s)
  (cond [(false? s) (...)]
        [else
         (... (school-name s)
              (school-tuition s)
              (fn-for-school (school-next s)))]))


;; Constants

(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define BAR-WIDTH 26)
(define BAR-COLOR "lightblue")
(define BAR-OUTLINE-COLOR "black")
(define Y-SCALE 1/100)


;; Functions

;; School -> Image
;; Produces the bar chart for the given schools

(define (bar-chart s)
  (cond [(false? s) (square 0 "solid" "black")]
        [else
         (beside/align
          "bottom"
          (overlay/align
           "center" "bottom"
           (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
           (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" BAR-OUTLINE-COLOR)
           (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR))
          (bar-chart (school-next s)))]))

;; Tests

(check-expect (bar-chart false) (square 0 "solid" "black"))
(check-expect (bar-chart (make-school "S1" 6000 false))
              (beside/align
               "bottom"
               (overlay/align
                "center" "bottom"
                (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "outline" BAR-OUTLINE-COLOR)
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "solid" BAR-COLOR))
               (square 0 "solid" "black")))
(check-expect (bar-chart (make-school "S2" 9000 (make-school "S1" 6000 false)))
              (beside/align
               "bottom"
               (overlay/align
                "center" "bottom"
                (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                (rectangle BAR-WIDTH (* 9000 Y-SCALE) "outline" BAR-OUTLINE-COLOR)
                (rectangle BAR-WIDTH (* 9000 Y-SCALE) "solid" BAR-COLOR))
               (overlay/align
                "center" "bottom"
                (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "outline" BAR-OUTLINE-COLOR)
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "solid" BAR-COLOR))
               (square 0 "solid" "black")))

;; Uncomment the below line to run this program
;; (bar-chart (make-school "S2" 9000 (make-school "S1" 6000 (make-school "S0" 7500 false))))
