;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname compound) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; compound-starter.rkt

;; PROBLEM:
;;
;; Design a data definition to represent hockey players, including both 
;; their first and last names.


(define-struct player (first-name last-name))
;; player is (make-player String String)
;; interp. player with two fields: first name and last name of the player in string



