;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

; ======================================================================
; PROBLEM A:
;
; You are designing a program to track a rocket's journey as it descends 
; 100 kilometers to Earth. You are only interested in the descent from 
; 100 kilometers to touchdown. Once the rocket has landed it is done.
;
; Design a data definition to represent the rocket's remaining descent. 
; Call it RocketDescent.
; ======================================================================

; RocketDescent is one of:
; - Number(0, 100]
; - false
; interp. Number of kilometers remaining to Earth, if false then the rocket has landed
(define RD1 100)
(define RD2 56)
(define RD3 0.1)
(define RD4 false)

; Template
#;
(define (fn-for-rocket-descent rd)
  (cond [(and (number? rd)
              (> rd 0)
              (<= rd 100)) (...)]
        [else (...)]))

; Template rules used
; one of: 2 cases
; - atomic non-distinct: Number(0, 100]
; - atomic distinct: false

;; =================
;; Functions:

; ======================================================================
; PROBLEM B:
;
; Design a function that will output the rocket's remaining descent distance 
; in a short string that can be broadcast on Twitter. 
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.
; ======================================================================

; RocketDescent -> String
; Produces a string which describes the descent of the rocket until it has landed
(check-expect (rocket-descent->msg 100) "The rocket is 100 kms from Earth")
(check-expect (rocket-descent->msg 40) "The rocket is 40 kms from Earth")
(check-expect (rocket-descent->msg 0.2) "The rocket is 1/5 kms from Earth")
(check-expect (rocket-descent->msg false) "The rocket has landed!")

;(define (rocket-descent->msg rd) "")  ;stub

(define (rocket-descent->msg rd)
  (cond [(and (number? rd)
              (> rd 0)
              (<= rd 100)) (string-append "The rocket is " (number->string rd) " kms from Earth")]
        [else "The rocket has landed!"]))