;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname growing-grass) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =======================================================================
;; PROBLEM:
;;
;; Design a world program as follows:
;;
;; The world starts off with a piece of grass waiting to grow. As time passes, 
;; the grass grows upwards. Pressing any key cuts the current strand of 
;; grass to 0, allowing a new piece to grow to the right of it.
;;
;; NOTE 1: Remember to follow the HtDW recipe! Be sure to do a proper domain 
;; analysis before starting to work on the code file.
;; =======================================================================

(require 2htdp/image)
(require 2htdp/universe)

;; ================
;; Constants:

(define WIDTH 610)
(define HEIGHT 400)
(define BG-COLOR "lightblue")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define GRASS-COLOR "green")
(define GRASS-WIDTH 20)
(define CLEARANCE 10)
(define GROWTH-RATE 2)


;; ================
;; Data definitions:

(define-struct grass (offset height))
;; Grass is (make-grass Natural[0, (WIDTH - GRASS-WIDTH - CLEARANCE)] Natural)
;; interp. represents the location and height of the grass at a particular moment:
;;   - offset: offset from the left edge upto the width of BACKGROUND
;;   - height: length of the grass from the bottom edge


;; ================
;; Function definitions:

;; Grass -> Grass
;; main function of the program; starts with (main (make-grass CLEARANCE 0))

(define (main grass)
  (big-bang grass               ; Grass
    (on-tick grow-grass)        ; Grass -> Grass
    (to-draw render-grass)      ; Grass -> Image
    (on-key handle-key)))       ; Grass KeyEvent -> Grass

;; Grass -> Grass
;; Increases the length of grass

(define (grow-grass grass)
  (make-grass
   (grass-offset grass)
   (+ (grass-height grass) GROWTH-RATE)))

;; Grass -> Image
;; Produces the image of the grass with it's length and offset 

(define (render-grass grass)
  (place-image
   (rectangle GRASS-WIDTH (grass-height grass) "solid" GRASS-COLOR)
   (+ (grass-offset grass) (/ GRASS-WIDTH 2))
   (- HEIGHT (/ (grass-height grass) 2))
   BACKGROUND))
  

;; Grass KeyEvent -> Grass
;; Cuts off the current grass and shifts it by some distance
;; If it goes out of the screen, it will reset the grass back to it's initial state

(define (handle-key grass ke)
  (cond [(key=? ke " ")
         (if (< (+ (grass-offset grass) GRASS-WIDTH CLEARANCE) WIDTH)
             (make-grass (+ (grass-offset grass) GRASS-WIDTH CLEARANCE) 0)
             (make-grass CLEARANCE 0))]
        [else grass]))

;; =================
;; Tests:

;; Test data for first case
(define G1 (make-grass CLEARANCE 0))
(define GI1 (rectangle GRASS-WIDTH (grass-height G1) "solid" GRASS-COLOR))
(define GX1 (+ (grass-offset G1) (/ GRASS-WIDTH 2)))
(define GY1 (- HEIGHT (/ (grass-height G1) 2)))

;; Test data for second case
(define G2 (make-grass 100 50))
(define GI2 (rectangle GRASS-WIDTH (grass-height G2) "solid" GRASS-COLOR))
(define GX2 (+ (grass-offset G2) (/ GRASS-WIDTH 2)))
(define GY2 (- HEIGHT (/ (grass-height G2) 2)))

;; Test for boundary condition
(define G3 (make-grass (- WIDTH (+ GRASS-WIDTH CLEARANCE)) 50))
(define G4 (make-grass (- WIDTH (+ GRASS-WIDTH CLEARANCE 10)) 50))

(check-expect (grow-grass G1) (make-grass CLEARANCE (+ 0 GROWTH-RATE)))
(check-expect (grow-grass G2) (make-grass 100 (+ 50 GROWTH-RATE)))

(check-expect (render-grass G1) (place-image GI1 GX1 GY1 BACKGROUND))
(check-expect (render-grass G2) (place-image GI2 GX2 GY2 BACKGROUND))

(check-expect (handle-key G1 " ") (make-grass (+ CLEARANCE GRASS-WIDTH CLEARANCE) 0))
(check-expect (handle-key G1 "a") G1)
(check-expect (handle-key G2 "b") G2)
(check-expect (handle-key G2 " ") (make-grass (+ 100 GRASS-WIDTH CLEARANCE) 0))
(check-expect (handle-key G3 " ") (make-grass CLEARANCE 0))
(check-expect (handle-key G4 " ") (make-grass (+ (grass-offset G4) GRASS-WIDTH CLEARANCE) 0))
