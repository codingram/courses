;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ==============================================================================
;; PROBLEM:
;;
;; Design an animation of a traffic light.
;;
;; Your program should show a traffic light that is red, then green,
;; then yellow, then red etc. For this program, your changing world
;; state data definition should be an enumeration.
;;
;; Here is what your program might look like if the initial world
;; state was the red traffic light:
;; Run the program to look at the image.
;;
;; Next is red, and so on.
;;
;; To make your lights change at a reasonable speed, you can use the
;; rate option to on-tick. If you say, for example, (on-tick next-color 1)
;; then big-bang will wait 1 second between calls to next-color.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain
;; analysis before starting to work on the code file.
;;
;; Note: If you want to design a slightly simpler version of the program,
;; you can modify it to display a single circle that changes color, rather
;; than three stacked circles.
;; ==============================================================================

(require 2htdp/image)
(require 2htdp/universe)

;; ==========
;; CONSTANTS

(define SPEED 1)
(define WIDTH 200)
(define HEIGHT 500)
(define BG-COLOR "black")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define CIRCLE-RADIUS (- (/ HEIGHT 8) 5))


;; ==========
;; DATA DEFINITIONS

;; LightState is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; interp. the color of the traffic light which is on currently

;; Mode is one of:
;; - "outline"
;; - "solid"
;; interp. mode of the circle image (traffic lights)


;; ==========
;; FUNCTION DEFINITIONS

;; LightState -> LightState
;; main function; start is (main "red")

(define (main ls)
  (big-bang ls                       ; LightState
    (on-tick change-state SPEED)     ; LightState -> LightState
    (to-draw render-state)))         ; LightState -> Image


;; LightState -> LightState
;; Produces the next state of the traffic light

(define (change-state ls)
  (cond [(string=? ls "red") "green"]
        [(string=? ls "yellow") "red"]
        [(string=? ls "green") "yellow"]))


;; Mode Mode Mode -> Image
;; Produces the image of the traffic light according to the specified mode for each color

(define (traffic-light-state red-mode yellow-mode green-mode)
  (place-image
   (circle CIRCLE-RADIUS red-mode "red")
   (/ WIDTH 2) (/ HEIGHT 4)
   (place-image
    (circle CIRCLE-RADIUS yellow-mode "yellow")
    (/ WIDTH 2) (/ HEIGHT 2)
    (place-image
     (circle CIRCLE-RADIUS green-mode "green")
     (/ WIDTH 2) (* (/ HEIGHT 4) 3)
     BACKGROUND))))


;; LightState -> Image
;; Produces the image to show the current state of the traffic light

(define (render-state ls)
  (cond [(string=? ls "red") (traffic-light-state "solid" "outline" "outline")]
        [(string=? ls "yellow") (traffic-light-state "outline" "solid" "outline")]
        [(string=? ls "green") (traffic-light-state "outline" "outline" "solid")]))


;; ==========
;; TESTS

(check-expect (change-state "red") "green")
(check-expect (change-state "yellow") "red")
(check-expect (change-state "green") "yellow")

(check-expect (render-state "red") (traffic-light-state "solid" "outline" "outline"))
(check-expect (render-state "yellow") (traffic-light-state "outline" "solid" "outline"))
(check-expect (render-state "green") (traffic-light-state "outline" "outline" "solid"))
