#lang htdp/isl

(require 2htdp/image)


;; PROBLEM:
;;
;; Design a function that consumes a number and produces a Sierpinski
;; triangle of that size. Your function should use generative recursion.
;;
;; One way to draw a Sierpinski triangle is to:
;;
;;  - start with an equilateral triangle with side length s
;;  - inside that triangle are three more Sierpinski triangles
;;  - and inside each of those... and so on
;;
;; So that you end up with something that looks like this:
;; (image removed)
;;
;; Note that in the 2nd picture above the inner triangles are drawn in
;; black and slightly smaller just to make them clear. In the real
;; Sierpinski triangle they should be in the same color and of side
;; length s/2. Also note that the center upside down triangle is not
;; an explicit triangle, it is simply formed from the other triangles.


;; Construct a three part termination argument for stri.
;;
;; Base case: (<= side CUTOFF)
;;
;; Reduction step: (/ side 2)
;;
;; Argument that repeated application of reduction step will eventually
;; reach the base case:
;; - As long as the CUTOFF value is > 0 and the side length starts at > 0, repeated
;;   application of the reduction step will eventually lead to the side value
;;   being less than the CUTOFF value, terminating the recursion.


;; The maximum size of the smallest triangle. After this there won't be any
;; recursive call
(define CUTOFF 10)

;; Number -> Image
;; Produce the Sierpinski Triangle of side length 'side'

(define (sierpinski-triangle side)
  (cond [(<= side CUTOFF) (triangle side "outline" "red")]
        [else
         (overlay (triangle side "outline" "red")
                  (letrec ([rec-tri (sierpinski-triangle (/ side 2))])
                    (above rec-tri
                           (beside rec-tri rec-tri))))]))


;; Tests:

(check-expect (sierpinski-triangle CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (sierpinski-triangle (* CUTOFF 2))
              (overlay (triangle (* CUTOFF 2) "outline" "red")
                       (letrec ([sub-tri (triangle CUTOFF "outline" "red")])
                         (above sub-tri
                                (beside sub-tri sub-tri)))))


;; PROBLEM:
;;
;; Design a function to produce a Sierpinski carpet of size s.
;;
;; Here is an example of a larger Sierpinski carpet.


;; Construct a three part termination argument for scarpet.
;;
;; Base case: (<= side CARPET-CUTOFF)
;;
;; Reduction step: (/ side 3)
;;
;; Argument that repeated application of reduction step will eventually
;; reach the base case:
;; - As long as the CARPET-CUTOFF value is > 0 and the side value starts at > 0, repeated
;;   application of the reduction step will eventually lead to the side value being
;;   less than the CARPET-CUTOFF value, terminating the recursion.


(define CARPET-CUTOFF 2)

;; Number -> Image
;; Produces the Sierpinski carpet of side length 'side'

(define (sierpinski-carpet side)
  (cond [(<= side CARPET-CUTOFF) (square side "outline" "red")]
        [else
         (overlay (square side "outline" "red")
                  (letrec ([sub-sqr (sierpinski-carpet (/ side 3))])
                    (above (beside sub-sqr sub-sqr sub-sqr)
                           (beside sub-sqr (square (/ side 3) "outline" "red") sub-sqr)
                           (beside sub-sqr sub-sqr sub-sqr))))]))


;; Tests:

(check-expect (sierpinski-carpet CARPET-CUTOFF) (square CARPET-CUTOFF "outline" "red"))
(check-expect (sierpinski-carpet (* CARPET-CUTOFF 3))
              (overlay (square (* CARPET-CUTOFF 3) "outline" "red")
                       (letrec ([sub-sqr (square CARPET-CUTOFF "outline" "red")])
                         (above (beside sub-sqr sub-sqr sub-sqr)
                                (beside sub-sqr (square CARPET-CUTOFF "outline" "red") sub-sqr)
                                (beside sub-sqr sub-sqr sub-sqr)))))
