;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dinner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

; =====================================================================
; PROBLEM A:
;
; You are working on a system that will automate delivery for 
; YesItCanFly! airlines catering service. 
; There are three dinner options for each passenger, chicken, pasta 
; or no dinner at all. 
;
; Design a data definition to represent a dinner order. Call the type 
; DinnerOrder.
; =====================================================================

; DinnerOrder is one of:
; - "chicken"
; - "pasta"
; - false
; interp. : name of the dish or if it's false then no dinner
(define DO1 "chicken")
(define DO2 "pasta")
(define DO3 false)

; Template
#;
(define (fn-for-dinner-order do)
  (cond [(false=? do) (...)]
        [(string=? do "chicken") (...)]
        [(string=? do "pasta") (...)]))

; Template rules used:
; one of: 3 cases
; - atomic distinct: "chicken"
; - atomic distinct: "pasta"
; - atomic distinct: false

;; =================
;; Functions:

; =====================================================================
; PROBLEM B:
;
; Design the function dinner-order-to-msg that consumes a dinner order 
; and produces a message for the flight attendants saying what the
; passenger ordered. 
;
; For example, calling dinner-order-to-msg for a chicken dinner would
; produce "The passenger ordered chicken."
; =====================================================================

; DinnerOrder -> String
; Produces a string which describes what the passenger ordered
(check-expect (dinner-order->msg "chicken") "The passenger ordered chicken")
(check-expect (dinner-order->msg "pasta") "The passenger ordered pasta")
(check-expect (dinner-order->msg false) "The passenger ordered no item")

;(define (dinner-order->msg do) "")  ;stub

(define (dinner-order->msg do)
  (cond [(false? do) "The passenger ordered no item"]  ;Fast return for false
        [(string=? do "chicken") "The passenger ordered chicken"]
        [(string=? do "pasta") "The passenger ordered pasta"]))

