;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; DESIGN a function called image-area that consumes an image and produces the 
; area of that image. For the area it is sufficient to just multiple the image's 
; width by its height.  Follow the HtDF recipe and leave behind commented 
; out versions of the stub and template.
; =====================================================================

(require 2htdp/image)

; Image -> Number
; Calculate the area of the image (multiply width by height)

;(define (image-area img) 0)        ;stub

;(define (image-area img)           ;template
;  (... img img))

(define (image-area img)
  (* (image-width img) (image-height img)))


(check-expect (image-area (rectangle 10 15 "solid" "red")) 150)
(check-expect (image-area (rectangle 5 2 "solid" "blue")) 10)
(check-expect (image-area (square 8 "solid" "white")) 64)
