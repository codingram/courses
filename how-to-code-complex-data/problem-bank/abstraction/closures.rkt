#lang htdp/isl

(require 2htdp/image)

;; closures-starter.rkt

;; Some setup data and functions to enable more interesting examples
;; below

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 30 20 "solid" "yellow"))
(define I3 (rectangle 40 50 "solid" "green"))
(define I4 (rectangle 60 50 "solid" "blue"))
(define I5 (rectangle 90 90 "solid" "orange"))

(define LOI1 (list I1 I2 I3 I4 I5))


;; NOTE: Unlike using-built-ins-starter.rkt this file does not define
;; the functions tall? wide? square? and area.



;; PROBLEM:
;;
;; Complete the design of the following functions by completing the body
;; which has already been templated to use a built-in abstract list function.

;; (listof Image) -> (listof Image)
;; produce list of only those images that have width >= height
(check-expect (wide-only (list I1 I2 I3 I4 I5)) (list I2 I4))

;; (define (wide-only loi) empty) ;stub

;; Again, I wanted to try how does 'letrec' works and it is basically a different version of
;; local where for the definition part, we don't have to use the define keyword. Instead, it
;; uses a special syntax where each name value/function pair is defined using square brackets.
;; For functions, we have to use lambda, otherwise it will be considered as a variable.
(define (wide-only loi)
  (letrec ([wide? (lambda (img) (> (image-width img) (image-height img)))])
    (filter wide? loi)))


;; Number (listof Image) -> (listof Image)
;; produce list of only those images in loi with width >= w
(check-expect (wider-than-only 40 LOI1) (list I4 I5))

;; (define (wider-than-only w loi) empty) ;stub

(define (wider-than-only width loi)
  (letrec ([wider? (lambda (img) (> (image-width img) width))])
    (filter wider? loi)))


;; (listof Number) -> (listof Number)
;; produce list of each number in lon cubed
(check-expect (cube-all (list 1 2 3)) (list (* 1 1 1) (* 2  2 2) (* 3 3 3)))

;; (define (cube-all lon) empty) ;stub

(define (cube-all lon)
  (letrec ([cube (lambda (num) (* num num num))])
    (map cube lon)))


;; String (listof String) -> (listof String)
;; produce list of all elements of los prefixed by p
(check-expect (prefix-all "accio " (list "portkey" "broom"))
              (list "accio portkey" "accio broom"))

;; (define (prefix-all p los) empty) ;stub

(define (prefix-all p los)
  (letrec ([prefix (lambda (str) (string-append p str))])
    (map prefix los)))
