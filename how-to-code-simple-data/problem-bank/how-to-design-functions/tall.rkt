;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; DESIGN a function that consumes an image and determines whether the 
;; image is tall.
;;
;; Remember, when we say DESIGN, we mean follow the recipe.
;;
;; Leave behind commented out versions of the stub and template.
;; =====================================================================

(require 2htdp/image)

;; Image -> Boolean
;; Returns true if height is greater than width otherwise false

;(define (tall img) false)   ;stub

;(define (tall img)
;;  (... img))

(define (tall img)
  (> (image-height img) (image-width img)))

(check-expect (tall (rectangle 10 15 "solid" "red")) true)
(check-expect (tall (rectangle 15 10 "solid" "blue")) false)
(check-expect (tall (square 10 "solid" "white")) false)