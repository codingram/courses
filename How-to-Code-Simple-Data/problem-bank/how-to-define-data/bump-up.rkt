;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bump-up) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; =====================================================================
; PROBLEM:
;
; Using the LetterGrade data definition below design a function that
; consumes a letter grade and produces the next highest letter grade. 
; Call your function bump-up.
; =====================================================================

;; Data definitions:

;; LetterGrade is one of: 
;;  - "A"
;;  - "B"
;;  - "C"
;; interp. the letter grade in a course
;; <examples are redundant for enumerations>
#;
(define (fn-for-letter-grade lg)
  (cond [(string=? lg "A") (...)]
        [(string=? lg "B") (...)]
        [(string=? lg "C") (...)]))

;; Template rules used:
;;  one-of: 3 cases
;;  atomic distinct: "A"
;;  atomic distinct: "B"
;;  atomic distinct: "C"


;; Functions:
; LetterGrade -> LetterGrade
; produces the next highest grade (remain same if its "A")
(check-expect (next-highest "A") "A")
(check-expect (next-highest "B") "A")
(check-expect (next-highest "C") "B")

; (define (next-highest lg) "A")    ;stub

(define (next-highest lg)
  (cond [(string=? lg "A") "A"]
        [(string=? lg "B") "A"]
        [(string=? lg "C") "B"]))