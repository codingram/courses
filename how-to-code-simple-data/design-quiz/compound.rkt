#lang htdp/bsl

#| PROBLEM:
 |
 | Design a World Program with Compound Data. You can be as creative as you like,
 | but keep it simple. Above all, follow the recipes! You must also stay within
 | the scope of the first part of the course. Do not use language features we have
 | not seen in the videos.
 |
 | If you need inspiration, you can choose to create a program that allows you to
 | click on a spot on the screen to create a flower, which then grows over time.
 | If you click again the first flower is replaced by a new one at the new
 | position.
 |
 | You should do all your design work in DrRacket. Following the step-by-step
 | recipe in DrRacket will help you be sure that you have a quality solution. |#

(require 2htdp/image)
(require 2htdp/universe)


; Constants

(define WIDTH 600)
(define HEIGHT 400)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define CENTER (circle 15 "solid" "lightyellow"))
(define PETAL (ellipse 30 50 "solid" "purple"))
(define FLOWER
  (overlay (above CENTER (rectangle 1 10 0 "white"))
           (overlay/align "center" "top"
                          (above (beside (rotate 216 PETAL)
                                         (rectangle 1 1 0 "white")
                                         (rotate 144 PETAL))
                                 (rotate 180 (beside (rotate 72 PETAL)
                                                     (rectangle 10 0 0 "white")
                                                     (rotate 288 PETAL))))
                          (above (rectangle 1 61 0 "white") PETAL))))


; Data definitions

(define-struct flower (x y size))
; Flower is (make-flower Integer Integer Natural)
; interp. x and y coordinate of the flower on BACKGROUND and the size of the flower
#;
(define (fn-for-flower f)
  (... (flower-x f)
       (flower-y f)
       (flower-size f)))


; Function definitions

; Flower -> Flower
; main function of the program, starts with (main ...)

(define (main f)
  (big-bang
      f                            ; Flower
    (to-draw render-flower)        ; Flower -> Image
    (on-tick grow-flower)          ; Flower -> Flower
    (on-mouse handle-mouse)))      ; Flower x y mevt -> Flower

; Flower -> Image
; Produces the current image of flower on BACKGROUND

(define (render-flower f)
  (place-image
    (which-image f)
    (flower-x f) (flower-y f) BACKGROUND))

; Flower -> Image
; Produces either an empty image or of flower as per the size given

(define (which-image f)
  (if (zero? (flower-size f))
    empty-image
    (flower-image f)))

; Flower -> Image
; Produces the image of the flower for given size
; Assumption: flower-size is greater than 0

(define (flower-image f)
  (rotate
    (remainder (flower-size f) 360)
    (scale (/ (flower-size f) 100) FLOWER)))

; Flower -> Flower
; Increases the size of the flower and rotates it as well

(define (grow-flower f)
  (make-flower (flower-x f)
               (flower-y f)
               (add1 (flower-size f))))

; Flower x y mevt -> Flower
; Removes the previous flower if there and grows flower at x and y coordinate
; of where the mouse clicked

(define (handle-mouse f x y mevt)
  (cond [(mouse=? mevt "button-down") (make-flower x y 0)]
        [else f]))


; Tests

(define F1 (make-flower 10 10 0))
(define F2 (make-flower 20 40 50))
(define F3 (make-flower 100 100 400))

(check-expect (render-flower F1)
              (place-image empty-image 10 10 BACKGROUND))
(check-expect (render-flower F2)
              (place-image (rotate 50 (scale (/ 50 100) FLOWER))
                           20 40 BACKGROUND))
(check-expect (render-flower F3)
              (place-image (rotate 40 (scale (/ 400 100) FLOWER))
                           100 100 BACKGROUND))

(check-expect (which-image F1) empty-image)
(check-expect (which-image F2)
              (rotate 50 (scale (/ 50 100) FLOWER)))

(check-expect (flower-image F3)
              (rotate 40 (scale (/ 400 100) FLOWER)))

(check-expect (grow-flower F1) (make-flower 10 10 1))
(check-expect (grow-flower F2) (make-flower 20 40 51))
(check-expect (grow-flower F3) (make-flower 100 100 401))

(check-expect (handle-mouse F1 50 50 "button-down") (make-flower 50 50 0))
(check-expect (handle-mouse F2 100 100 "move") F2)
(check-expect (handle-mouse F3 10 40 "button-down") (make-flower 10 40 0))


; Run the program
(main (make-flower 100 100 0))
