;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-to-display) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; You are asked to contribute to the design for a very simple New Year's
;; Eve countdown display. You already have the data definition given below. 
;; You need to design a function that consumes Countdown and produces an
;; image showing the current status of the countdown. 
;; =====================================================================

(require 2htdp/image)

;; Data definitions:

;; Countdown is one of:
;;  - false
;;  - Natural[1, 10]
;;  - "complete"
;; interp.
;;    false           means countdown has not yet started
;;    Natural[1, 10]  means countdown is running and how many seconds left
;;    "complete"      means countdown is over
(define CD1 false)
(define CD2 10)          ;just started running
(define CD3  1)          ;almost over
(define CD4 "complete")
#;
(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (<= 1 c) (<= c 10)) (... c)]
        [else (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: false
;;  - atomic non-distinct: Natural[1, 10]
;;  - atomic distinct: "complete"

;; Functions:
;; Countdown -> Image
;; produces an image for the current state of the Countdown
(check-expect (countdown-img false) (text "Countdown not started" 16 "red"))
(check-expect (countdown-img 1) (text (number->string 1) 16 "green"))
(check-expect (countdown-img 10) (text (number->string 10) 16 "green"))
(check-expect (countdown-img "complete") (text "Happy New Year!" 16 "white"))

;(define (countdown-img c) (square 0 "solid" "white"))  ;stub

(define (countdown-img c)
  (cond [(false? c)
         (text "Countdown not started" 16 "red")]
        [(and (number? c) (<= 1 c) (<= c 10))
         (text (number->string c) 16 "green")]
        [else
         (text "Happy New Year!" 16 "white")]))