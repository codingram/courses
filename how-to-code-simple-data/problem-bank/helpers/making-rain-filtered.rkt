#lang htdp/bsl


#| PROBLEM:
 |
 | Design a simple interactive animation of rain falling down a screen. Wherever we click,
 | a rain drop should be created and as time goes by it should fall. Over time the drops
 | will reach the bottom of the screen and "fall off". You should filter these excess
 | drops out of the world state - otherwise your program is continuing to tick and
 | and draw them long after they are invisible.
 |
 | In your design pay particular attention to the helper rules. In our solution we use
 | these rules to split out helpers:
 |   - function composition
 |   - reference
 |   - knowledge domain shift
 |
 |
 | NOTE: This is a fairly long problem.  While you should be getting more comfortable with
 | world problems there is still a fair amount of work to do here. Our solution has 9
 | functions including main. If you find it is taking you too long then jump ahead to the
 | next homework problem and finish this later. |#


(require 2htdp/image)
(require 2htdp/universe)


; Constants:

(define WIDTH  500)
(define HEIGHT 500)
(define DROP-SPEED 2)
(define DROP-WIDTH 8)
(define DROP-HEIGHT 20)
(define DROP-COLOR "blue")
(define BG-COLOR "light blue")
(define WATER-DROP (ellipse DROP-WIDTH DROP-HEIGHT "solid" DROP-COLOR))
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" BG-COLOR))


; Data definitions:

(define-struct drop (x y))
; Drop is (make-drop Integer Integer)
; interp. A raindrop on the screen, with x and y coordinates.

#;
(define (fn-for-drop d)
  (... (drop-x d)
       (drop-y d)))

; Template Rules used:
; - compound: 2 fields


; ListOfDrop is one of:
;  - empty
;  - (cons Drop ListOfDrop)
; interp. a list of drops

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

; Template Rules used:
; - one-of: 2 cases
; - atomic distinct: empty
; - compound: (cons Drop ListOfDrop)
; - reference: (first lod) is Drop
; - self reference: (rest lod) is ListOfDrop


; Functions:

; ListOfDrop -> ListOfDrop
; start rain program by evaluating (main empty)

(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
; if mevt is "button-down" add a new drop at that position

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


; ListOfDrop -> ListOfDrop
; produce filtered and ticked list of drops

(define (next-drops lod)
  (move-drops (filter-drops lod)))

; ListOfDrop -> ListOfDrop
; Remove the drops which are outside the BACKGROUND

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (is-drop-outside? (first lod))
           (filter-drops (rest lod))
           (cons (first lod) (filter-drops (rest lod))))]))

; Drop -> Boolean
; Determines whether the drop is outside the BACKGROUND

(define (is-drop-outside? d)
  (>= (drop-y d) (+ HEIGHT (/ DROP-HEIGHT 2))))

; ListOfDrop -> ListOfDrop
; Move the y coordinate of all the drops at DROP-SPEED

(define (move-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (move-single-drop (first lod))
               (move-drops (rest lod)))]))

; Drop -> Drop
; Move a single drop by DROP-SPEED

(define (move-single-drop d)
  (make-drop (drop-x d) (+ (drop-y d) DROP-SPEED)))

; ListOfDrop -> Image
; Render the drops onto BACKGROUND

(define (render-drops lod)
  (cond [(empty? lod) BACKGROUND]
        [else
         (place-drop-on (first lod)
                        (render-drops (rest lod)))]))

; Drop Image -> Image
; Produces an image with the given drop on it

(define (place-drop-on d img)
  (place-image WATER-DROP
               (drop-x d) (drop-y d) img))


; Tests
; D - Drops, ND - Next drops

(define D1 (make-drop 0 0))
(define ND1 (make-drop 0 DROP-SPEED))
(define D2 (make-drop 50 100))
(define ND2 (make-drop 50 (+ 100 DROP-SPEED)))
(define D3 (make-drop 100 (- HEIGHT DROP-SPEED)))  ; Edge of BACKGROUND
(define ND3 (make-drop 100 HEIGHT))
(define D4 (make-drop 200 (+ HEIGHT DROP-HEIGHT 10)))  ; outside BACKGROUND

(define LOD0 empty)  ; Empty case
(define LOD1 (cons D2 empty))
(define LOD2 (cons D2 (cons D3 (cons D1 empty))))
(define LOD3 (cons D3 empty))  ; Edge case
(define LOD4 (cons D3 (cons D4 empty)))  ; Edge case + outside case

(check-expect (place-drop-on D2 BACKGROUND)
              (place-image WATER-DROP 50 100 BACKGROUND))

(check-expect (render-drops LOD0) BACKGROUND)
(check-expect (render-drops LOD1)
              (place-image WATER-DROP 50 100 BACKGROUND))
(check-expect (render-drops LOD2)
              (place-image WATER-DROP 50 100
                           (place-image WATER-DROP 100 (- HEIGHT DROP-SPEED)
                                        (place-image WATER-DROP 0 0
                                                     BACKGROUND))))

(check-expect (handle-mouse LOD0 100 100 "move") LOD0)
(check-expect (handle-mouse LOD0 50 100 "button-down") LOD1)
(check-expect (handle-mouse LOD2 100 (- HEIGHT DROP-SPEED) "button-down") (cons D3 LOD2))
(check-expect (handle-mouse LOD2 10 10 "button-up") LOD2)

(check-expect (next-drops LOD0) LOD0)
(check-expect (next-drops LOD1) (cons ND2 empty))
(check-expect (next-drops LOD2) (cons ND2 (cons ND3 (cons ND1 empty))))
(check-expect (next-drops LOD3) (cons ND3 empty))
(check-expect (next-drops LOD4) (cons ND3 empty)) ; Filter out D4

(check-expect (filter-drops LOD0) LOD0)
(check-expect (filter-drops LOD2) LOD2)
(check-expect (filter-drops LOD4) LOD3)

(check-expect (move-drops LOD0) empty)
(check-expect (move-drops LOD1) (cons ND2 empty))
(check-expect (move-drops LOD2) (cons ND2 (cons ND3 (cons ND1 empty))))
(check-expect (move-drops LOD3) (cons ND3 empty))

(check-expect (is-drop-outside? D1) false)
(check-expect (is-drop-outside? D3) false)
(check-expect (is-drop-outside? D4) true)

(check-expect (move-single-drop D1) ND1)
(check-expect (move-single-drop D2) ND2)
(check-expect (move-single-drop D3) ND3)

; Uncomment the below line to start the program
; (main empty)
