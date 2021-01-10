#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)


;; PROBLEM:
;;
;; A Cantor Set is another fractal with a nice simple geometry.
;; The idea of a Cantor set is to have a bar (or rectangle) of
;; a certain width w, then below that are two recursive calls each
;; of 1/3 the width, separated by a whitespace of 1/3 the width.
;;
;; So this means that the
;;   width of the whitespace   wc  is  (/ w 3)
;;   width of recursive calls  wr  is  (/ (- w wc) 2)
;;
;; To make it look better a little extra whitespace is put between
;; the bars.
;;
;;
;; Here are a couple of examples (assuming a reasonable CUTOFF)
;;
;; (cantor CUTOFF) produces:
;;
;;
;; (cantor (* CUTOFF 3)) produces:
;;
;;
;; And that keeps building up to something like the following. So
;; as it goes it gets wider and taller of course.
;;
;;
;; PROBLEM A:
;;
;; Design a function that consumes a width and produces a cantor set of
;; the given width.
;;
;;
;; PROBLEM B:
;;
;; Add a second parameter to your function that controls the percentage
;; of the recursive call that is white each time. Calling your new function
;; with a second argument of 1/3 would produce the same images as the old
;; function.
;;
;; PROBLEM C:
;;
;; Now you can make a fun world program that works this way:
;;   The world state should simply be the most recent x coordinate of the mouse.
;;
;;   The to-draw handler should just call your new cantor function with the
;;   width of your MTS as its first argument and the last x coordinate of
;;   the mouse divided by that width as its second argument.



;; ==========
;; Constants

(define WIDTH 400)
(define CUTOFF 5)
(define BAR-HEIGHT 20)
(define BAR-COLOR 'blue)
(define INTERMEDIATE-DISTANCE (/ BAR-HEIGHT 2))


;; ============
;; Functions:

;; WorldState -> WorldState
;; Main function to run the cantor set generator; start with (main 0)

(define (main state)
  (big-bang state
    (to-draw render)
    (on-mouse handle-mouse)))


;; WorldState -> Image
;; Renders the image of the current world state

(define (render state)
  (cantor WIDTH (/ state WIDTH)))


;; WorldState Number Number MouseEvent -> WorldState
;; Produce the next worldstate if the mouse moves

(define (handle-mouse state x y mouse-event)
  (cond [(mouse=? mouse-event "move") x]
        [else state]))


;; Number Number -> Image
;; Produce the image of the cantor set of given width
;; Space between the recursive structure can be controlled with the space argument
;; which should be in (0.0 1.0)

(define (cantor width space)
  (cond [(<= width CUTOFF) (cantor-bar width)]
        [else
         (letrec ([space-width (* width space)]
                  [next-width (/ (- width space-width) 2)]
                  [next-cantor (cantor next-width space)])
           (above (cantor-bar width)
                  (dummy-rectangle width)
                  (beside next-cantor (dummy-rectangle space-width) next-cantor)))]))


;; Number -> Image
;; Produce a dummy rectangle of the specified width of solid white color
;; This is mainly used as a space between each cantor step

(define (dummy-rectangle width)
  (rectangle width INTERMEDIATE-DISTANCE 'solid 'white))


;; Number -> Image
;; Produce the cantor bar image for the specified width
;; Convenient function to produce the cantor bar as 3 out of 4 arguments are constants

(define (cantor-bar width)
  (rectangle width BAR-HEIGHT 'solid BAR-COLOR))


;; ============================
;; Termination argument:
;;
;; Trivial case: width of the cantor bar is less than CUTOFF
;;
;; Reduction step: the top bar is split into three pieces and the recursion is called
;;                 on the width of the first and third piece
;;
;; Argument: As we will be reducing the width of the bar which is the argument passed
;;           to the recursion call, at some point the width will be less than CUTOFF,
;;           terminating the recursion

;; ========
;; Tests

(define W1 CUTOFF)
(define W2 (* 3 CUTOFF))
(define W3 (* 2 W2))

(check-expect (dummy-rectangle W2) (rectangle W2 INTERMEDIATE-DISTANCE 'solid 'white))
(check-expect (cantor-bar W2) (rectangle W2 BAR-HEIGHT 'solid BAR-COLOR))

(check-expect (cantor W1 (/ 1 3)) (cantor-bar W1))
(check-expect (cantor W2 (/ 1 3))
              (letrec ([space-width (/ W2 3)]
                       [next-width (/ (- W2 space-width) 2)])
                (above (cantor-bar W2)
                       (dummy-rectangle W2)
                       (beside (cantor-bar next-width)
                               (dummy-rectangle space-width)
                               (cantor-bar next-width)))))
(check-expect (cantor W3 0.2)
              (letrec ([space-width (* W3 0.2)]
                       [next-width (/ (- W3 space-width) 2)])
                (above (cantor-bar W3)
                       (dummy-rectangle W3)
                       (letrec ([other-width (/ (- next-width (* next-width 0.2)) 2)])
                         (beside (above (cantor-bar next-width)
                                        (dummy-rectangle next-width)
                                        (beside (cantor-bar other-width)
                                                (dummy-rectangle (* next-width 0.2))
                                                (cantor-bar other-width)))
                                 (dummy-rectangle space-width)
                                 (above (cantor-bar next-width)
                                        (dummy-rectangle next-width)
                                        (beside (cantor-bar other-width)
                                                (dummy-rectangle (* next-width 0.2))
                                                (cantor-bar other-width))))))))

(check-expect (render 100) (cantor WIDTH (/ 100 WIDTH)))
(check-expect (render 300) (cantor WIDTH (/ 300 WIDTH)))
(check-expect (handle-mouse 0   0 10 "move") 0)
(check-expect (handle-mouse 100 0 10 "move") 0)
(check-expect (handle-mouse  20 0 10 "button-down") 20)

(main 0)
