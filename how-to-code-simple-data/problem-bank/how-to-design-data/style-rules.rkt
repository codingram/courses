;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname style-rules) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ======================================================================
;; PROBLEM:
;;
;; You're redesigning the SeatNum data definition from lecture, and you're not
;; sure if you've done it correctly. When you ask a TA for feedback, she tells
;; you that you haven't followed our style rules and she asks you to re-format 
;; your data definition before she gives you feedback.
;;
;; a) Why is it important to follow style rules?
;;
;; b) Fix the data definition below so that it follows our style rules. Be sure to 
;; consult the style rules page so that you make ALL the required changes, of which 
;; there are quite a number.
;; ======================================================================

;SeatNum is Natural[1,32]
;interp. The number of a seat in a row.
;Seats 1 and 32 are aisle seats.
(define SN1 1)
(define SN2 32)

#;
(define (fn-for-sear-number sn)
  (... sn)) 

;; template rules used:
;; - atomic non-distinct: Natural[1,32]
