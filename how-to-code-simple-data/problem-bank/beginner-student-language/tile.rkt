;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tile) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; PROBLEM:
;;
;; Use the DrRacket square, beside and above functions to create an image like this one:
;; Run the program to produce the image
;;
;; If you prefer to be more creative feel free to do so. You can use other DrRacket image 
;; functions to make a more interesting or more attractive image.

(require 2htdp/image)

(define (color-square color)
  (square 30 "solid" color))

(define blue-square (color-square "blue"))
(define yellow-square (color-square "yellow"))

(above (beside blue-square yellow-square)
       (beside yellow-square blue-square))
