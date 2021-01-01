;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname even-more-arithmetic-expression) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =============================================================================
;; PROBLEM: 
;;
;; Write the BSL expression that represents the arithmetic expression:     
;; (6 + 3) * (9 - 7)
;; which equals 18.  Do not just write 18! Instead, write a BSL expression that clearly 
;; mirrors the arithmetic expression.
;; =============================================================================

(* (+ 6 3) (- 9 7))s