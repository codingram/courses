;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname triangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; PROBLEM:
;
; Write an expression that uses triangle, overlay, and rotate to produce an image similar to this:
; Run the program to see the image
;
; You can consult the DrRacket help desk for information on how to use triangle and overlay.
; Don't worry about the exact size of the triangles.


(require 2htdp/image)

(define yellow-tri (triangle 50 "solid" "yellow"))
(define green-tri (triangle 50 "solid" "green"))

(overlay green-tri (rotate 180 yellow-tri))