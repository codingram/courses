;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ==============================================================
; PROBLEM:
;
; Use the How to Design Worlds recipe to design an interactive
; program in which a cat starts at the left edge of the display 
; and then walks across the screen to the right. When the cat
; reaches the right edge it should just keep going right off 
; the screen.
;
; Once your design is complete revise it to add a new feature,
; which is that pressing the space key should cause the cat to
; go back to the left edge of the screen. When you do this, go
; all the way back to your domain analysis and incorporate the
; new feature.
;
; To help you get started, here is a picture of a cat, which we
; have taken from the 2nd edition of the How to Design Programs 
; book on which this course is based.
; ==============================================================

(require 2htdp/image)
(require 2htdp/universe)
(require "images/cat.rkt")

;; CAT-IMG

;; ===============
;; CONSTANTS

(define WIDTH 800)
(define HEIGHT 400)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define CAT-Y (/ HEIGHT 2))
(define CAT-SPEED 5)


;; ===============
;; Data definitions

;; Cat-x is Number
;; interp. x coordinate of the cat


;; ===============
;; Function definitions

;; Cat-x -> Cat-x
;; main function for the project, start main with (main 0)

(define (main cx)
  (big-bang cx                   ; Cat-x
    (on-tick advance-cat)        ; Cat-x -> Cat-x
    (to-draw render-cat)         ; Cat-x -> Image
    (on-key handle-key)          ; Cat-x KeyEvent -> Cat-x
    (on-mouse handle-mouse)))    ; Cat-x x-coord y-coord KeyEvent -> Cat-x


;; Cat-x -> Cat-x
;; produce the next cat, advancing it by 1 pixel to right

(define (advance-cat cx)
  (+ cx CAT-SPEED))


;; Cat-x -> Image
;; produce the image of the next cat to be displayed

(define (render-cat cx)
  (place-image CAT-IMG cx CAT-Y BACKGROUND))


;; Cat-x KeyEvent -> Cat-x
;; reset cat to left edge when space key is pressed

(define (handle-key cx ke)
  (cond [(key=? ke " ") 0]
        [else cx]))

;; Cat-x x-coord y-coord KeyEvent -> Cat-x
;; change the cat position to where the mouse x coordinate is when the mouse is clicked (button-down)

(define (handle-mouse cx x y me)
  (cond [(mouse=? me "button-down") x]
        [else cx]))


;; ==========
;; TESTS

(check-expect (advance-cat 4) (+ 4 CAT-SPEED))
(check-expect (advance-cat 0) (+ 0 CAT-SPEED))

(check-expect (render-cat WIDTH) (place-image CAT-IMG WIDTH CAT-Y BACKGROUND))
(check-expect (render-cat 0) (place-image CAT-IMG 0 CAT-Y BACKGROUND))
(check-expect (render-cat (/ WIDTH 2)) (place-image CAT-IMG (/ WIDTH 2) CAT-Y BACKGROUND))

(check-expect (handle-key 50 " ") 0)
(check-expect (handle-key 50 "d") 50)
(check-expect (handle-key 0 " ") 0)
(check-expect (handle-key 0 "e") 0)

(check-expect (handle-mouse 20 50 100 "button-down") 50)
(check-expect (handle-mouse 40 40 HEIGHT "button-down") 40)
(check-expect (handle-mouse 0 400 100 "button-down") 400)
(check-expect (handle-mouse 600 0 (/ HEIGHT 2) "button-down") 0)
