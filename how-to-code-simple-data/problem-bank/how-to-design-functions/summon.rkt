;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname summon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =====================================================================
;; PROBLEM:
;;
;; Design a function that generates a summoning charm. For example: 
;;
;;    (summon "Firebolt") should produce "accio Firebolt"
;;    (summon "portkey")  should produce "accio portkey"
;;    (summon "broom")    should produce "accio broom"
;;   
;; See http://harrypotter.wikia.com/wiki/Summoning_Charm for background on
;; summoning charms.
;;
;; Remember, when we say DESIGN, we mean follow the recipe.
;;
;; Follow the HtDF recipe and leave behind commented out versions of the stub and template. 
;; =====================================================================

;; String -> String
;; Produces a string with 'accio' at the start of it for the summoning charm

;(define (summon spell) "")    ;stub

;(define (summon spell)
;;  (... spell))

(define (summon spell)
  (string-append "accio" " " spell))

(check-expect (summon "Firebolt") "accio Firebolt")
(check-expect (summon "portkey") "accio portkey")
(check-expect (summon "broom") "accio broom")