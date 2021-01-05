#lang htdp/isl

(require 2htdp/image)


;; PROBLEM :
;;
;; Design a function that will create the following fractal:
;; (image removed)
;;
;; Each circle is surrounded by circles that are two-fifths smaller.
;;
;; You can build these images using the convenient beside and above functions
;; if you make your actual recursive function be one that just produces the
;; top leaf shape. You can then rotate that to produce the other three shapes.
;;
;; You don't have to use this structure if you are prepared to use more
;; complex place-image functions and do some arithmetic. But the approach
;; where you use the helper is simpler.
;;
;; Include a termination argument for your design.


;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 10)


;; Number -> Image
;; Produces the circle fractal image for the radius provided.

(define (circle-fractal radii)
  (cond [(<= radii TRIVIAL-SIZE) (circle radii "solid" "blue")]
        [else
         (letrec ([sub-radii (* STEP radii)]
                  [sub-circle (circle-fractal sub-radii)]
                  ; When one of the leaf is placed from the offset of the center
                  ; circle, the center of the total image will be different.
                  ; This variable is the distance from the center of this new image
                  ; to its edge which is the half of the sum of:
                  ; - diameter of the center circle
                  ; - radius of the sub circle
                  ; - half of the width of the leaf image
                  ;   (Remember: the entire image is symmetric, so it doesn't matter
                  ;   whether you take the image-width or image-height)
                  [center-distance (/ (+ (* 2 radii)
                                         sub-radii
                                         (/ (image-width sub-circle) 2))
                                      2)])
           (overlay/offset
            sub-circle (+ center-distance sub-radii) 0    ; left leaf
            (overlay/offset
             sub-circle (- (+ sub-radii radii)) 0         ; right leaf
             (overlay/offset
              sub-circle 0 (+ center-distance sub-radii)  ; top leaf
              (overlay/offset
               sub-circle 0 (- (+ sub-radii radii))       ; bottom leaf
               (circle radii "solid" "blue"))))))]))


;; =========================
;; Termination argument:
;;
;; Trivial case: (<= radii TRIVIAL-SIZE); radii is less than or equal to TRIVIAL-SIZE
;;
;; Reduction step: (* STEP radii); decrease the next leaf size by STEP
;;
;; Argument: As we keep on reducing the size of the leaves in the reduction step,
;;           at one point the size will become less than TRIVIAL-SIZE, terminating the
;;           recursion.

;; =======
;; Tests:

(check-expect (circle-fractal TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue"))

(check-expect (circle-fractal (/ (* TRIVIAL-SIZE 5) 2))
              (letrec ([center-radii (/ (* TRIVIAL-SIZE 5) 2)]
                       [sub-radii TRIVIAL-SIZE]  ; Just an alias for better readability
                       [sub-circle (circle TRIVIAL-SIZE "solid" "blue")]
                       [center-distance (/ (+ (* 2 center-radii)
                                              sub-radii
                                              (/ (image-width sub-circle) 2))
                                           2)])
                (overlay/offset
                 sub-circle (+ center-distance sub-radii) 0
                 (overlay/offset
                  sub-circle (- (+ TRIVIAL-SIZE center-radii)) 0
                  (overlay/offset
                   sub-circle 0 (+ center-distance sub-radii)
                   (overlay/offset
                    sub-circle 0 (- (+ sub-radii center-radii))
                    (circle center-radii "solid" "blue")))))))
