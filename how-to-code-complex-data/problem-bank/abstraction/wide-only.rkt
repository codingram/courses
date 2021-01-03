#lang htdp/isl

(require 2htdp/image)

;; wide-only-starter.rkt


;; PROBLEM:
;;
;; Use the built in version of filter to design a function called wide-only
;; that consumes a list of images and produces a list containing only those
;; images that are wider than they are tall.

;; =========================================
;; Same problem as in using-built-ins.rkt
;; =========================================

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 30 20 "solid" "yellow"))
(define I3 (rectangle 40 50 "solid" "green"))
(define I4 (rectangle 60 50 "solid" "blue"))
(define I5 (rectangle 90 90 "solid" "orange"))

(define (wide?   img) (> (image-width img) (image-height img)))

;; (listof Image) -> (listof Image)
;; produce list of only those images that are wide?
(check-expect (wide-only (list I1 I2 I3 I4 I5)) (list I2 I4))

;; (define (wide-only loi) empty) ;stub

(define (wide-only loi)
  (filter wide? loi))
