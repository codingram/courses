;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boxify) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; Use the How to Design Functions (HtDF) recipe to design a function that consumes an image, 
; and appears to put a box around it. Note that you can do this by creating an "outline" 
; rectangle that is bigger than the image, and then using overlay to put it on top of the image. 
;
; Remember, when we say DESIGN, we mean follow the recipe.
;
; Leave behind commented out versions of the stub and template.
; =====================================================================

(require 2htdp/image)

; Image -> Image
; Put the given image in a box whose size is greater than the input image

;(define (boxify img) img)    ;stub

(define (boxify img)
  (overlay img (rectangle (image-width img) (image-height img) "outline" "yellow")))

(boxify (ellipse 60 30 "solid" "red"))
