#lang htdp/bsl

#| PROBLEM:
 |
 | In this problem imagine you have a bunch of pictures that you would like to
 | store as data and present in different ways. We'll do a simple version of that
 | here, and set the stage for a more elaborate version later.
 |
 | (A) Design a data definition to represent an arbitrary number of images.
 |
 | (B) Design a function called arrange-images that consumes an arbitrary number
 |     of images and lays them out left-to-right in increasing order of size. |#


(require 2htdp/image)


;; Data definition

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
          (... (first loi)
               (fn-for-loi (rest loi)))]))


;; Function definition

;; ListOfImage -> Image
;; Produces all the images from left-to-right in increasing order of size

(define (arrange-images loi)
  (layout-images (sort-images loi)))

;; ListOfImage -> ListOfImage
;; Produces a list of sorted images in increasing order of their area

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
          (insert-image (first loi)
                        (sort-images (rest loi)))]))

;; Image ListOfImage -> ListOfImage
;; Insert image in proper place in the list of images
;; in increasing order of size
;; Assumption: loi is already sorted

(define (insert-image img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
          (if (larger-image? img (first loi))
            (cons (first loi)
                  (insert-image img (rest loi)))
            (cons img loi))]))

;; Image Image -> Boolean
;; Produces true if the first image is larger than the second in sie
;; otherwise false
;; Produces false if the areas are same

(define (larger-image? img1 img2)
  (> (image-area img1) (image-area img2)))

;; Image -> Number
;; Produces the area of a given image

(define (image-area img)
  (* (image-width img) (image-height img)))

;; ListOfImage -> Image
;; Lay down all the images from left to right

(define (layout-images loi)
  (cond [(empty? loi) empty-image]
        [else
          (beside (first loi)
                  (layout-images (rest loi)))]))

;; Tests

(define I1 (rectangle 50 30 "solid" "red"))   ; 1500
(define I2 (rectangle 20 50 "solid" "blue"))  ; 1000
(define I3 (square 40 "solid" "white"))       ; 1600

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))
(define LOI4 (cons I2 (cons I3 (cons I1 empty))))

(check-expect (image-area I1) 1500)
(check-expect (image-area I2) 1000)
(check-expect (image-area I3) 1600)

(check-expect (larger-image? I1 I2) true)
(check-expect (larger-image? I2 I3) false)
(check-expect (larger-image? I1 I1) false)

(check-expect (insert-image I1 empty) (cons I1 empty))
(check-expect (insert-image I3 (cons I2 (cons I1 empty)))
              (cons I2 (cons I1 (cons I3 empty))))
(check-expect (insert-image I2 LOI2) (cons I2 (cons I1 empty)))

(check-expect (sort-images LOI1) empty)
(check-expect (sort-images LOI2) (cons I1 empty))
(check-expect (sort-images LOI3) (cons I2 (cons I1 empty)))
(check-expect (sort-images LOI4) (cons I2 (cons I1 (cons I3 empty))))

(check-expect (layout-images LOI1) empty-image)
(check-expect (layout-images LOI2) (beside I1 empty-image))
(check-expect (layout-images LOI3) (beside I1 I2 empty-image))
(check-expect (layout-images LOI4) (beside I2 I3 I1 empty-image))

(check-expect (arrange-images LOI1) empty-image)
(check-expect (arrange-images LOI2) (beside I1 empty-image))
(check-expect (arrange-images LOI3) (beside I2 I1 empty-image))
(check-expect (arrange-images LOI4) (beside I2 I1 I3 empty-image))
