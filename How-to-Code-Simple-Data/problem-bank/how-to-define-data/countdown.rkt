;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; Consider designing the system for controlling a New Year's Eve
; display. Design a data definition to represent the current state 
; of the countdown, which falls into one of three categories: 
;
;  - not yet started
;  - from 10 to 1 seconds before midnight
;  - complete (Happy New Year!)
; =====================================================================

; Data definitions

; Countdown is one of:
; - false
; - Natural[1, 10]
; - "complete"
; interp.
;  false : countdown hasn't started
;  Natural[1, 10] : countdown is running and it at this number
;  complete : countdown is over
(define CD1 false)
(define CD2 1)
(define CD3 10)
(define CD4 "complete")

; Template
(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (>= 1) (<= 10)) (... c)]
        [else (...)]))

; Template rules used:
; - one of: 3 cases
; - atomic distinct: false
; - atomic non-distinct: Natural[1, 10]
; - atomic distinct: "complete"