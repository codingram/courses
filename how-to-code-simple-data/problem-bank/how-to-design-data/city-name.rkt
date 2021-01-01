;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname city-name) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; Imagine that you are designing a program that, among other things, 
;; has information about the names of cities in its problem domain.
;;
;; Design a data definition to represent the name of a city. 
;; ===================================================================== 

;; Data definitions

;; CityName is String
;; interp. name of a city
(define CN1 "New York")
(define CN2 "Mumbai")

;; Template
#;
(define (fn-for-city-name cn)
  (... cn))

