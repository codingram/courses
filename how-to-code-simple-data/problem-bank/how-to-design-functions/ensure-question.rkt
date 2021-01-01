;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ensure-question) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; Use the How to Design Functions (HtDF) recipe to design a function that consumes a string, and adds "?" 
;; to the end unless the string already ends in "?".
;;
;; For this question, assume the string has length > 0. Follow the HtDF recipe and leave behind commented 
;; out versions of the stub and template.
;; =====================================================================

;; String -> String
;; Adds a '?' at the end of the string if it doesn't exist assuming string has length > 0

;(define (question str) "x")   ;stub

;(define (question str)        ;template
;;  (... str))

(define (question str)
  (if (string=? (string-ith str (- (string-length str) 1)) "?")
      str
      (string-append str "?")))

(check-expect (question "hello") "hello?")
(check-expect (question "Are you there?") "Are you there?")
(check-expect (question "No, are you") "No, are you?")
(check-expect (question "a") "a?")
(check-expect (question "In the middle? Like seriously") "In the middle? Like seriously?")
(check-expect (question "IgNoRE CaSE") "IgNoRE CaSE?")