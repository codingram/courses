;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cflag) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =============================================================================
;; PROBLEM:
;;
;; The background for the Canadian Flag (without the maple leaf) is this:
;; Run to program to produce the image
;;         
;; Write an expression to produce that background. (If you want to get the
;; details right, officially the overall flag has proportions 1:2, and the 
;; band widths are in the ratio 1:2:1.)
;; =============================================================================

(require 2htdp/image)

(define red-rect (rectangle 20 40 "solid" "red"))
(define white-square (square 40 "solid" "white"))

(beside red-rect white-square red-rect)