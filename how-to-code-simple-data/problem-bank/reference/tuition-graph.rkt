#lang htdp/bsl


#| PROBLEM:
 |
 | Eva is trying to decide where to go to university. One important factor for her is
 | tuition costs. Eva is a visual thinker, and has taken Systematic Program Design,
 | so she decides to design a program that will help her visualize the costs at
 | different schools. She decides to start simply, knowing she can revise her design
 | later.
 |
 | The information she has so far is the names of some schools as well as their
 | international student tuition costs. She would like to be able to represent that
 | information in bar charts like this one (removed):
 |
 | (A) Design data definitions to represent the information Eva has.
 | (B) Design a function that consumes information about schools and their
 |     tuition and produces a bar chart.
 | (C) Design a function that consumes information about schools and produces
 |     the school with the lowest international student tuition. |#


(require 2htdp/image)


;; Constants

(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define BAR-WIDTH 26)
(define BAR-COLOR "lightblue")
(define BAR-OUTLINE-COLOR "black")
(define Y-SCALE 1/100)


;; Data definitions

(define-struct school (name tuition))
;; School is (make-school String Natural)
;; interp. name of the school and tuition for that school in INR
#;
(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))

;; ListOfSchool is one of:
;; - empty
;; - (cons School ListOfSchool)
;; interp. a list of schools
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))


;; Function definitions

;; School -> Image
;; Produces a single bar for the given school

(define (make-bar s)
  (overlay/align
   "center" "bottom"
   (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
   (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" BAR-OUTLINE-COLOR)
   (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))

;; ListOfSchool -> Image
;; Produces the bar chart showing names and tuition of consumed schools

(define (bar-chart los)
  (cond [(empty? los) (square 0 "solid" "black")]
        [else
         (beside/align "bottom" (make-bar (first los))
                       (bar-chart (rest los)))]))


;; Tests

(define S1 (make-school "S1" 6000))
(define S2 (make-school "S2" 9000))
(define S3 (make-school "S3" 7400))
(define S4 (make-school "S4" 11500))
(define S5 (make-school "S5" 8500))

(check-expect (bar-chart empty) (square 0 "solid" "black"))
(check-expect (bar-chart (cons S1 empty))
              (beside/align
               "bottom"
               (overlay/align
                "center" "bottom"
                (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "outline" BAR-OUTLINE-COLOR)
                (rectangle BAR-WIDTH (* 6000 Y-SCALE) "solid" BAR-COLOR))
               (square 0 "solid" "black")))
(check-expect (bar-chart (cons S2 (cons S1 empty)))
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

(check-expect (make-bar S1)
              (overlay/align
               "center" "bottom"
               (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
               (rectangle BAR-WIDTH (* 6000 Y-SCALE) "outline" BAR-OUTLINE-COLOR)
               (rectangle BAR-WIDTH (* 6000 Y-SCALE) "solid" BAR-COLOR)))

;; Uncomment the below line to see the demo
(bar-chart (cons S1 (cons S2 (cons S3 (cons S4 (cons S5 empty))))))
