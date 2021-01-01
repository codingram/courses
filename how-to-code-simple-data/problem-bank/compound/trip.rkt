;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; ==================================================================
;; PROBLEM A:
;;
;; Design a data definition to help travellers plan their next trip. 
;; A trip should specify an origin, destination, mode of transport and 
;; duration (in days).
;; ==================================================================

(define-struct trip (origin destination mode-of-transport duration))
;; Trips is (make-trip String String String Natural)
;; interp. data about the trip including:
;;  - origin: origin place of the trip in String
;;  - destination: destination place of the trip in String
;;  - mode-of-transport: transportation mode for the trip in String
;;  - duration: period from start to end in days (Natural)

;; =================
;; Functions:

;; ==================================================================
;; PROBLEM B:
;;
;; You have just found out that you have to use all your days off work 
;; on your next vacation before they expire at the end of the year. 
;; Comparing two options for a trip, you want to take the one that 
;; lasts the longest. Design a function that compares two trips and 
;; returns the trip with the longest duration.
;;
;; Note that the rule for templating a function that consumes two 
;; compound data parameters is for the template to include all 
;; the selectors for both parameters.
;; ==================================================================

;; Trip Trip -> Trip
;; Returns the trip with the longest duration
;; If the duration is the same, it returns the first trip

(define (longest-trip t1 t2)
  (if (>= (trip-duration t1) (trip-duration t2))
      t1
      t2))

;; ===============
;; Tests:

(define T1 (make-trip "a" "b" "c" 4))
(define T2 (make-trip "b" "a" "d" 2))
(define T3 (make-trip "c" "d" "e" 4))
(define T4 (make-trip "d" "f" "g" 8))

(check-expect (longest-trip T1 T2) T1)
(check-expect (longest-trip T2 T3) T3)
(check-expect (longest-trip T1 T3) T1)
(check-expect (longest-trip T4 T2) T4)
(check-expect (longest-trip T3 T1) T3)
