;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname yell) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; DESIGN a function called yell that consumes strings like "hello" 
; and adds "!" to produce strings like "hello!".
;
; Remember, when we say DESIGN, we mean follow the recipe.
;
; Leave behind commented out versions of the stub and template.
; =====================================================================

;; Signature: String -> String
;; Purpose: Add "!" to the end of the input string

;(define (yell str) "")             ; stub

;(define (yell str)                 ; template
;  (... str ...))

(define (yell str)
  (string-append str "!"))

(check-expect (yell "hello") "hello!")
(check-expect (yell "HEY") "HEY!")
(check-expect (yell "Damn") "Damn!")
