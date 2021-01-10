#lang htdp/isl

(require 2htdp/image)


;; PROBLEM:
;;
;; First review the discussion of the Van Koch Line fractal at:
;; http://pages.infinit.net/garrick/fractals/.
;;
;; Now design a function to draw a SIMPLIFIED version of the fractal.
;;
;; For this problem you will draw a simplified version as follows:
;; (image removed)
;;
;; Notice that the difference here is that the vertical parts of the
;; curve, or segments BC and DE in this figure
;; are just ordinary lines they are not themselves recursive Koch curves.
;; That ends up making things much simpler in terms of the math required
;; to draw this curve.
;;
;; We want you to make the function consume positions using
;; DrRacket's posn structure. A reasonable data definition for these
;; is included below.
;;
;; The signature and purpose of your function should be:
;;
;;     Posn Posn Image -> Image
;;     Add a simplified Koch fractal to image of length ln, going from p1 to p2
;;     length ln is calculated by (distance p1 p2)
;;     Assume p1 and p2 have same y-coordinate.
;;
;;     (define (vkline p1 p2 img) img) ;stub
;;
;; Include a termination argument for your function.
;;
;; We've also given you some constants and two other functions
;; below that should be useful.


;; Create a simplified Van Koch Line fractal.

;; =================
;; Constants:

(define LINE-CUTOFF 5)

(define WIDTH 300)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:

;(define-struct posn (x y))   ;struct is already part of racket
;; Posn is (make-posn Number Number)
;; interp. A cartesian position, x and y are screen coordinates.
(define TP1 (make-posn 20 30))
(define TP2 (make-posn 100 10))

;; =================
;; Functions:

;; Posn Posn -> Number
;; produce the distance between two points
(check-expect (distance TP1 TP1) 0)
(check-within (distance TP1 TP2) 82.4621125 0.0000001)

(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p2) (posn-x p1)))
           (sqr (- (posn-y p2) (posn-y p1))))))


;; Posn Posn Image -> Image
;; add a black line from p1 to p2 on image
(check-expect (simple-line TP1 TP2 BACKGROUND)
              (add-line BACKGROUND 20 30 100 10 "black"))

(define (simple-line p1 p2 img)
  (add-line img (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) "black"))


;; Posn Posn Image -> Image
;; Add a simplified Koch fractal to image of length ln, going from p1 to p2
;; length ln is calculated by (distance p1 p2)
;; Assume p1 and p2 have same y-coordinate.

(define (vkline p1 p2 img)
  (letrec ([p-distance (distance p1 p2)])
    (cond [(<= p-distance LINE-CUTOFF) (simple-line p1 p2 img)]
          [else
           (letrec ([sub-distance (/ p-distance 3)]
                    [A p1]
                    [Ax (posn-x p1)]
                    [Ay (posn-y p1)]
                    [B (make-posn (+ Ax sub-distance) Ay)]
                    [C (make-posn (+ Ax sub-distance) (- Ay sub-distance))]
                    [D (make-posn (+ Ax (* 2 sub-distance)) (- Ay sub-distance))]
                    [E (make-posn (+ Ax (* 2 sub-distance)) Ay)]
                    [F p2])
             (vkline
              A B
              (simple-line
               B C
               (vkline
                C D
                (simple-line
                 D E
                 (vkline
                  E F img))))))])))


;; =========================
;; Termination argument:
;;
;; Trivial case: Distance between the two points p1 and p2 is less than LINE-CUTOFF
;;
;; Reduction step: Split the distance into three pieces and call the function on the
;;                 the endpoints for the pieces
;;
;; Argument: reduction steps reduces the distance between the points which will eventually
;;           lead to the condition of trivial case where the distance is less than the
;;           LINE-CUTOFF, terminating the recursion


;; ========
;; Tests:

(define P1 (make-posn 0 HEIGHT))
(define P2 (make-posn LINE-CUTOFF HEIGHT))
(define P3 (make-posn (* 3 LINE-CUTOFF) HEIGHT))

(check-expect (vkline P1 P2 BACKGROUND)
              (simple-line P1 P2 BACKGROUND))
(check-expect (vkline P1 P3 BACKGROUND)
              (letrec ([sub-distance (/ (distance P1 P3) 3)]
                       [A P1]
                       [B (make-posn sub-distance HEIGHT)]
                       [C (make-posn sub-distance (- HEIGHT sub-distance))]
                       [D (make-posn (* 2 sub-distance) (- HEIGHT sub-distance))]
                       [E (make-posn (* 2 sub-distance) HEIGHT)]
                       [F (make-posn (* 3 sub-distance) HEIGHT)])
                (vkline
                 A B
                 (simple-line
                  B C
                  (vkline
                   C D
                   (simple-line
                    D E
                    (vkline
                     E F BACKGROUND)))))))
