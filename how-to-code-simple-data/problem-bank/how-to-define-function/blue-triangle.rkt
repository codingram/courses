;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blue-triangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; Design a function that consumes a number and produces a blue solid triangle of that size.
;
; You should use The How to Design Functions (HtDF) recipe, and your complete design should include
; signature, purpose, commented out stub, examples/tests, commented out template and the completed function.
; =====================================================================

(require 2htdp/image)

; Number -> Image (Triangle)
; Produces a triangles of length as input number

; (define (blue-triangle size) (triangle 1 "solid" "blue"))    ;stub

; (define (blue-triangle size)
;   (... image)

(define (blue-triangle size)
  (triangle size "solid" "blue"))

(check-expect (blue-triangle 15) (triangle 15 "solid" "blue"))
(check-expect (blue-triangle 30) (triangle 30 "solid" "blue"))