;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tracker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ==============================================================
; PROBLEM:
;
; Design a world program that displays the current (x, y) position
; of the mouse at that current position. So as the mouse moves the 
; numbers in the (x, y) display changes and its position changes. 
; ==============================================================

(require 2htdp/image)
(require 2htdp/universe)

; ===============
; Constants

(define WIDTH 500)
(define HEIGHT WIDTH)
(define BG-COLOR "black")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define TEXT-SIZE 20)
(define TEXT-COLOR "white")


; ===============
; Data definitions

(define-struct pointer (x y))
; Pointer is (make-pointer Natural Natural)
; interp. the x and y coordinates of the mouse pointer


; ===============
; Function definitions

; Pointer -> Pointer
; main function of the program; starts with (main (make-pointer 0 0))

(define (main pt)
  (big-bang pt                    ; Pointer
    (to-draw render-coords)       ; Pointer -> Image
    (on-mouse handle-mouse)))     ; Pointer x-coord y-coord MouseEvent -> Pointer

; Pointer -> Image
; Produces the image of x and y coordinate on the background

(define (render-coords pt)
  (place-image
   (text (string-append "("
                        (number->string (pointer-x pt)) ", "
                        (number->string (pointer-y pt)) ")") TEXT-SIZE TEXT-COLOR)
   (pointer-x pt) (pointer-y pt)
   BACKGROUND))

; Pointer x-coord y-coord MouseEvent -> Pointer
; Change the position of the text image wherever the mouse moves to

(define (handle-mouse pt x y me)
  (cond [(mouse=? me "move")
         (make-pointer x y)]
        [else pt]))


; ===================
; Tests:

(define P1 (make-pointer 0 0))
(define P2 (make-pointer 100 150))

(check-expect (render-coords P1)
              (place-image (text "(0, 0)" TEXT-SIZE TEXT-COLOR) 0 0 BACKGROUND))
(check-expect (render-coords P2)
              (place-image (text "(100, 150)" TEXT-SIZE TEXT-COLOR) 100 150 BACKGROUND))

(check-expect (handle-mouse P1 10 20 "button-down") P1)
(check-expect (handle-mouse P2 50 30 "move") (make-pointer 50 30))