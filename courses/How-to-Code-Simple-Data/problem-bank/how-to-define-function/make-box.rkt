;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname make-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; You might want to create boxes of different colours.
;
; Use the How to Design Functions (HtDF) recipe to design a function that consumes a color, and creates a 
; solid 10x10 square of that colour.  Follow the HtDF recipe and leave behind commented out versions of
; the stub and template.
; =====================================================================

(require 2htdp/image)

; String (Color) -> Image (Square)
; Takes a string which is a color name and produces a square of 10x10 of the input color

; (define (color-square color) (square 1 "solid" "blue"))

(define (color-square color)
  (square 10 "solid" color))

(check-expect (color-square "blue") (square 10 "solid" "blue"))
(check-expect (color-square "white") (square 10 "solid" "white"))
(check-expect (color-square "red") (square 10 "solid" "red"))