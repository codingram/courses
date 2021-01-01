;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname letter-grade) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; As part of designing a system to keep track of student grades, you
;; are asked to design a data definition to represent the letter grade 
;; in a course, which is one of A, B or C.
;; =====================================================================

;; StudentGrade is one of:
;; - "A"
;; - "B"
;; - "C"
;; interp. Grades of a student
(define SG1 "A")
(define SG2 "B")
(define SG3 "C")

;; Template
(define (fn-for-student-grade sg)
  (... sg))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: "A"
;; - atomic distinct: "B"
;; - atomic distinct: "C"
