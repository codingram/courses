;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname less-than-five) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; DESIGN function that consumes a string and determines whether its length is
; less than 5.  Follow the HtDF recipe and leave behind commented out versions 
; of the stub and template.
; =====================================================================

; String -> Boolean
; Determines whether the length of string is less than 5 or not

; (define (less-than-5 str) false)       ;stub

(define (less-than-5 str)
  (< (string-length str) 5))

(check-expect (less-than-5 "hello") false)
(check-expect (less-than-5 "testing") false)
(check-expect (less-than-5 "") true)
(check-expect (less-than-5 "test") true)
  