;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname compare-images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =============================================================================
; PROBLEM:
;
; Based on the two constants provided, write three expressions to determine whether: 
;
; 1) IMAGE1 is taller than IMAGE2
; 2) IMAGE1 is narrower than IMAGE2
; 3) IMAGE1 has both the same width AND height as IMAGE2
; =============================================================================

(require 2htdp/image)

(define (rect width height)
  (rectangle width height "solid" "red"))

(define IMAGE1 (rect 10 15))
(define IMAGE2 (rect 15 10))

(if (> (image-height IMAGE1) (image-height IMAGE2))
    "IMAGE1 is taller than IMAGE2"
    "IMAGE2 is taller than IMAGE1")

(if (< (image-width IMAGE1) (image-width IMAGE2))
    "IMAGE1 is narrower than IMAGE2"
    "IMAGE2 is narrower than IMAGE1")

; defining two more images to test equality
(define IMAGE3 (rect 20 40))
(define IMAGE4 (rect 20 40))

(if (and (= (image-width IMAGE3) (image-width IMAGE4))
         (= (image-height IMAGE3) (image-height IMAGE3)))
    "IMAGE3 has both the same width and height as IMAGE4"
    "")



