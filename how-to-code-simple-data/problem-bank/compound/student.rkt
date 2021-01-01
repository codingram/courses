;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; student-starter.rkt

;; =================
;; Data definitions:

;; PROBLEM A:
;;
;; Design a data definition to help a teacher organize their next field trip. 
;; On the trip, lunch must be provided for all students. For each student, track 
;; their name, their grade (from 1 to 12), and whether or not they have allergies.


(define-struct student (name grade allergies?))
;; Student is (make-student String Number Boolean)
;; interp. Data about a single student which includes:
;;    - name: name of the student in String
;;    - grade: grade of the student in Number
;;    - allergies?: true if they have allergies otherwise false

;; =================
;; Functions:


;; PROBLEM B:
;;
;; To plan for the field trip, if students are in grade 6 or below, the teacher 
;; is responsible for keeping track of their allergies. If a student has allergies, 
;; and is in a qualifying grade, their name should be added to a special list. 
;; Design a function to produce true if a student name should be added to this list.


;; Student -> Boolean
;; Produces true if the student has allergies and is in grade 6 or below

(define (add-to-list? s)
  (and (student-allergies? s) (<= (student-grade s) 6)))


;; ============
;; TESTS

(define S1 (make-student "Raj" 4 false))
(define S2 (make-student "Akash" 3 true))
(define S3 (make-student "Sanjay" 9 false))
(define S4 (make-student "Dom" 10 true))
(define S5 (make-student "Harsh" 6 false))
(define S6 (make-student "Ron" 6 true))

(check-expect (add-to-list? S1) false)
(check-expect (add-to-list? S2) true)
(check-expect (add-to-list? S3) false)
(check-expect (add-to-list? S4) false)
(check-expect (add-to-list? S5) false)
(check-expect (add-to-list? S6) true)