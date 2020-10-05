;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =========================================================================
; PROBLEM:
;
; Design an animation of a simple countdown. 
;
; Your program should display a simple countdown, that starts at ten, and
; decreases by one each clock tick until it reaches zero, and stays there.
;
; To make your countdown progress at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, 
; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
; calls to advance-countdown.
;
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
;
; Once you are finished the simple version of the program, you can improve
; it by reseting the countdown to ten when you press the spacebar.
; =========================================================================

(require 2htdp/image)
(require 2htdp/universe)

;; ==============
;; CONSTANTS

(define TICK-RATE 1)
(define WIDTH 300)
(define HEIGHT 300)
(define BG-COLOR "black")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define TEXT-SIZE 150)
(define TEXT-COLOR "white")


;; ==============
;; Data definitions

;; Countdown is Natural[0, 10]
;; interp. current countdown value


;; ==============
;; Function definitions

;; Countdown -> Countdown
;; main function, starts with (main 10)

(define (main cd)
  (big-bang cd                             ; Countdown
    (on-tick advance-countdown TICK-RATE)  ; Countdown -> Countdown
    (to-draw render-countdown)             ; Countdown -> Image
    (on-key handle-key)))                  ; Countdown KeyEvent -> Countdown


;; Countdown -> Countdown
;; Decreases the countdown value by 1 per tick of the clock

(define (advance-countdown cd)
  (if (= cd 0)
      0
      (- cd 1)))

;; Countdown -> Image
;; Produces the image to be displayed for the current countdown value

(define (render-countdown cd)
  (place-image
   (text (number->string cd) TEXT-SIZE TEXT-COLOR)
   (/ WIDTH 2)
   (/ HEIGHT 2)
   BACKGROUND))

;; Countdown KeyEvent -> Countdown
;; Resets the countdown to 10 when spacebar is pressed

(define (handle-key cd ke)
  (cond [(key=? ke " ") 10]
        [else cd]))


;; ===========
;; TESTS

(check-expect (advance-countdown 10) 9)
(check-expect (advance-countdown 5) 4)
(check-expect (advance-countdown 1) 0)
(check-expect (advance-countdown 0) 0)

(check-expect (render-countdown 10) (place-image
                                     (text (number->string 10) TEXT-SIZE TEXT-COLOR)
                                     (/ WIDTH 2)
                                     (/ HEIGHT 2)
                                     BACKGROUND))
(check-expect (render-countdown 0) (place-image
                                    (text (number->string 0) TEXT-SIZE TEXT-COLOR)
                                    (/ WIDTH 2)
                                    (/ HEIGHT 2)
                                    BACKGROUND))

(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key 4 " ") 10)
(check-expect (handle-key 4 "a") 4)
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 0 "g") 0)