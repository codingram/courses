;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; DESIGN a function called area that consumes the length of one side 
;; of a square and produces the area of the square.
;;
;; Remember, when we say DESIGN, we mean follow the recipe.
;;
;; Leave behind commented out versions of the stub and template.
;; =====================================================================

;; Number -> Number
;; Produces the area of the square given the length of one of its side

;(define (square-area side) 0)        ;stub

;(define (square-area side)           ;template
;;  (... side))

(define (square-area side)
  (* side side))

(check-expect (square-area 4) 16)
(check-expect (square-area 10) 100)
(check-expect (square-area 1) 1)