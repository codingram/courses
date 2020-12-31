#lang htdp/bsl

(require 2htdp/image)


;; PROBLEM:
;;
;; Complete the design of a hierarchical image organizer.  The information and data
;; for this problem are similar to the file system example in the fs-starter.rkt file.
;; But there are some key differences:
;;   - this data is designed to keep a hierchical collection of images
;;   - in this data a directory keeps its sub-directories in a separate list from
;;     the images it contains
;;   - as a consequence data and images are two clearly separate types
;;
;; Start by carefully reviewing the partial data definitions below.


;; =================
;; Constants:

(define LABEL-SIZE 18)
(define LABEL-COLOR "white")


;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 10 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty empty))           ; empty directory
(define D5 (make-dir "D5" empty (list I1 I2)))
(define D6 (make-dir "D6" (list D4) (list I3)))
(define D7 (make-dir "D7" (list D5 D6) empty))


;; PART A:
;;
;; Annotate the type comments with reference arrows and label each one to say
;; whether it is a reference, self-reference or mutual-reference.
;;
;; PART B:
;;
;; Write out the templates for Dir, ListOfDir and ListOfImage. Identify for each
;; call to a template function which arrow from part A it corresponds to.

;; Templates:
#;
(define (fn-for-dir dir)
  (... (dir-name dir)  ; String
       (fn-for-list-of-dir (dir-sub-dirs dir))
       (fn-for-list-of-image (dir-images dir))))
#;
(define (fn-for-list-of-dir list-of-dir)
  (cond [(empty? list-of-dir) (...)]
        [else
          (... (fn-for-dir (first list-of-dir))  ; dir
               (fn-for-list-of-dir (rest list-of-dir)))]))
#;
(define (fn-for-list-of-image list-of-image)
  (cond [(empty? list-of-image) (...)]
        [else
          (... (first list-of-image)  ; Image
               (fn-for-list-of-image (rest list-of-image)))]))


;; =================
;; Functions:


;; PROBLEM B:
;;
;; Design a function to calculate the total size (width * height) of all the images
;; in a directory and its sub-directories.

;; Dir -> Integer
;; ListOfDir -> Integer
;; ListOfImage -> Integer
;; Produces the total sum of the area of images in the given directory and its sub directories.


(define (total-size-dir dir)
  (+ (total-size-list-of-dir (dir-sub-dirs dir))
     (total-size-list-of-image (dir-images dir))))

(define (total-size-list-of-dir list-of-dir)
  (cond [(empty? list-of-dir) 0]
        [else
          (+ (total-size-dir (first list-of-dir))
             (total-size-list-of-dir (rest list-of-dir)))]))

(define (total-size-list-of-image list-of-image)
  (cond [(empty? list-of-image) 0]
        [else
          (+ (image-area (first list-of-image))
             (total-size-list-of-image (rest list-of-image)))]))

;; Image -> Natural
;; Calculate the area of the provided image

(define (image-area image)
  (* (image-width image) (image-height image)))

;; Tests

(check-expect (image-area I1) 100)
(check-expect (total-size-dir D4) 0)
(check-expect (total-size-list-of-dir empty) 0)
(check-expect (total-size-list-of-image empty) 0)
(check-expect (total-size-list-of-image (list I1 I2)) 200)
(check-expect (total-size-list-of-dir (list D5)) 200)
(check-expect (total-size-list-of-dir (list D5 D6)) 382)
(check-expect (total-size-dir D5) 200)
(check-expect (total-size-dir D6) 182)
(check-expect (total-size-dir D7) 382)


;; PROBLEM C:
;;
;; Design a function to produce rendering of a directory with its images. Keep it
;; simple and be sure to spend the first 10 minutes of your work with paper and
;; pencil!

;; Dir -> Image
;; ListOfDir -> Image
;; ListOfImage -> Image
;; Produces a simple rendering of all the images in the given directory


(define (render-dir dir)
  (above (text (dir-name dir) LABEL-SIZE LABEL-COLOR)
         (render-list-of-dir (dir-sub-dirs dir))
         (render-list-of-image (dir-images dir))))

(define (render-list-of-dir list-of-dir)
  (cond [(empty? list-of-dir) empty-image]
        [else
          (beside (render-dir (first list-of-dir))
                  (render-list-of-dir (rest list-of-dir)))]))

(define (render-list-of-image list-of-image)
  (cond [(empty? list-of-image) empty-image]
        [else
          (above (first list-of-image)
                 (render-list-of-image (rest list-of-image)))]))


;; Tests

(check-expect (render-list-of-image empty) empty-image)
(check-expect (render-list-of-dir empty) empty-image)
(check-expect (render-list-of-image (list I1 I2)) (above I1 I2 empty-image))
(check-expect (render-dir D5)
              (above (text "D5" LABEL-SIZE LABEL-COLOR)
                     (render-list-of-image (list I1 I2))))
(check-expect (render-dir D7)
              (above (text "D7" LABEL-SIZE LABEL-COLOR)
                     (beside (render-dir D5) (render-dir D6) empty-image)))
