#lang htdp/isl

(require 2htdp/image)

;; ellipses-starter.rkt

;; =================
;; Constants:

(define BLANK (square 0 "outline" "white"))

;; =================
;; Functions:


;; PROBLEM A:
;;
;; Use build-list to write an expression (an expression, not a function) to
;; produce a list of 20 ellipses ranging in width from 0 to 19.
;;
;; NOTE: Assuming n refers to a number, the expression
;; (ellipse n (* 2 n) "solid" "blue") will produce an ellipse twice as tall
;; as it is wide.


;; Extracting the list of images to be used in later part of the code
(define ellipses (letrec ([build-ellipse (lambda (n) (ellipse n (* 2 n) "solid" "blue"))])
  (build-list 19 build-ellipse)))


;; Using local
#;
(local
  [(define (build-ellipse n)
     (ellipse n (* 2 n) "solid" "blue"))]
  (build-list 19 build-ellipse))


;; PROBLEM B:
;;
;; Write an expression using one of the other built-in abstract functions
;; to put the ellipses beside each other in a single image like this:
;;
;; Image: All ellipses are lined up beside each other in ascending order.
;;
;; HINT: If you are unsure how to proceed, first do part A, and then design a
;; traditional function operating on lists to do the job. Then think about
;; which abstract list function to use based on that.


(foldr beside BLANK ellipses)


;; PROBLEM C:
;;
;; By just using a different built in list function write an expression
;; to put the ellipses beside each other in a single image like this:
;;
;; Image: All ellipses are lined up beside each other in descending order.


(foldl beside BLANK ellipses)
