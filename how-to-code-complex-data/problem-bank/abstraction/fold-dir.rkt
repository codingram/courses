#lang htdp/isl

(require 2htdp/image)


;; In this exercise you will be need to remember the following DDs
;; for an image organizer.


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
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:


;; PROBLEM A:
;;
;; Design an abstract fold function for Dir called fold-dir.


;; (String Y Z -> X) (X Y -> Y) (Image Z -> Z) Y Z Dir -> X
;; Abtract function for the Dir

(define (fold-dir func1 func2 func3 base1 base2 dir)
  (local
    [(define (fn-for-dir dir)  ; Dir -> X
       (func1 (dir-name dir)
              (fn-for-lod (dir-sub-dirs dir))
              (fn-for-loi (dir-images dir))))
     (define (fn-for-lod lod)  ; (listof Dir) -> Y
       (cond [(empty? lod) base1]
             [else
               (func2 (fn-for-dir (first lod))
                      (fn-for-lod (rest lod)))]))
     (define (fn-for-loi loi)  ; (listof Image) -> Z
       (cond [(empty? loi) base2]
             [else
               (func3 (first loi)
                      (fn-for-loi (rest loi)))]))]
    (fn-for-dir dir)))


;; PROBLEM B:
;;
;; Design a function that consumes a Dir and produces the number of
;; images in the directory and its sub-directories.
;; Use the fold-dir abstract function.

;; Dir -> Number
;; Produces the number of images in the directory and its sub-directories

(define (total-images dir)
  (local
    [(define (total-count dirname total-subdir-images total-dir-images)
       (+ total-subdir-images total-dir-images))
     (define (count-images img total) (+ 1 total))]
    (fold-dir total-count + count-images 0 0 dir)))


(check-expect (total-images D4) 2)
(check-expect (total-images D5) 1)
(check-expect (total-images D6) 3)


;; PROBLEM C:
;;
;; Design a function that consumes a Dir and a String. The function looks in
;; dir and all its sub-directories for a directory with the given name. If it
;; finds such a directory it should produce true, if not it should produce false.
;; Use the fold-dir abstract function.

;; String Dir -> Boolean
;; Produces true if a directory with the given name exist in the Dir and sub-dirs, false otherwise.

(define (dir-exists? name dir)
  (local
    [(define (func1 dirname lod-res loi-res)
       (or (string=? dirname name)
         lod-res
         loi-res))
     (define (func2 dir-res lod-res) (or dir-res lod-res))
     (define (func3 img loi-res) false)]
    (fold-dir func1 func2 func3 false false dir)))


(check-expect (dir-exists? "D4" D4) true)
(check-expect (dir-exists? "D4" D6) true)
(check-expect (dir-exists? "D1" D6) false)


;; PROBLEM D:
;;
;; Is fold-dir really the best way to code the function from part C? Why or
;; why not?

;; No.
;; We don't get the short circuit behaviour. Even if the function finds the name in the
;; tree, it won't stop looking. It will traverse the entire tree to every nodes and leaves
;; and then only it will return the answer, whatever it might be. Even if the node we are
;; looking for is right at the beginning, it will still traverse the entire tree and only then
;; return true.
