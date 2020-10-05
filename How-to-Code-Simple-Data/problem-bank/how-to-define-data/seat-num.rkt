;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname seat-num) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ======================================================================
; PROBLEM:
;
; Imagine that you are designing a program to manage ticket sales for a
; theatre. (Also imagine that the theatre is perfectly rectangular in shape!) 
;
; Design a data definition to represent a seat number in a row, where each 
; row has 32 seats. (Just the seat number, not the row number.)
; ======================================================================

; SeatNumber is Natural[1, 32]
; interp. Represents the seat number in a row
(define SN1 1)
(define SN2 24)
(define SN3 32)

; Template rules used:
; - atomic-non-distinct: Natural[1, 32]

#;
(define (fn-for-seat-number sn)
  (... sn))