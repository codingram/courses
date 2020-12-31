#lang htdp/bsl

(require 2htdp/image)


;; Data definitions:
;; =================

(define-struct element (name data sub-elements))
;; Element is (make-element String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or sub-elements.
;;         If data is 0, then sub-elements is considered to be list of sub elements.
;;         If data is not 0, then sub-elements is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-element "F1" 1 empty))
(define F2 (make-element "F2" 2 empty))
(define F3 (make-element "F3" 3 empty))
(define D4 (make-element "D4" 0 (list F1 F2)))
(define D5 (make-element "D5" 0 (list F3)))
(define D6 (make-element "D6" 0 (list D4 D5)))


;; Templates:
;; ==========
#;
(define (fn-for-element element)
  (... (element-name element)
       (element-data element)
       (fn-for-list-of-element (element-sub-elements element))))
#;
(define (fn-for-list-of-element list-of-element)
  (cond [(empty? list-of-element) (...)]
        [else
          (... (fn-for-element (first list-of-element))
               (fn-for-list-of-element (rest list-of-element)))]))


;; Functions:
;; ==========

;; PROBLEM
;; =======
;;
;; Design a function that consumes Element and produces the sum of all the file data in
;; the tree.

;; Element -> Integer
;; ListOfElement -> Integer
;; Produces the sum of file data in the given element (arbitrarily arity tree)

(define (sum-data-element element)
  (if (zero? (element-data element))
    (sum-data-list-of-element (element-sub-elements element))
    (element-data element)))



(define (sum-data-list-of-element list-of-element)
  (cond [(empty? list-of-element) 0]
        [else
          (+ (sum-data-element (first list-of-element))
             (sum-data-list-of-element (rest list-of-element)))]))


;; Tests

(check-expect (sum-data-element F1) 1)
(check-expect (sum-data-list-of-element empty) 0)
(check-expect (sum-data-element D5) 3)
(check-expect (sum-data-element D4) 3)
(check-expect (sum-data-element D6) 6)


;; PROBLEM
;; =======
;;
;; Design a function that consumes Element and produces a list of the names of all the elements in
;; the tree.


;; Element -> ListOfString
;; ListOfElement -> ListOfString
;; Produces the list of names of all the elements in the tree.


(define (all-names-element element)
  (append (list (element-name element))
          (all-names-list-of-element (element-sub-elements element))))


(define (all-names-list-of-element list-of-element)
  (cond [(empty? list-of-element) empty]
        [else
          (append (all-names-element (first list-of-element))
                  (all-names-list-of-element (rest list-of-element)))]))


;; Tests

(check-expect (all-names-element F1) (list "F1"))
(check-expect (all-names-list-of-element empty) empty)
(check-expect (all-names-element D5) (list "D5" "F3"))
(check-expect (all-names-element D4) (list "D4" "F1" "F2"))
(check-expect (all-names-list-of-element (list D4 D5)) (list "D4" "F1" "F2" "D5" "F3"))
(check-expect (all-names-element D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))


;; PROBLEM
;; =======
;;
;; Design a function that consumes String and Element and looks for a data element with the given
;; name. If it finds that element it produces the data, otherwise it produces false.

;; String Element -> Integer or false
;; String ListOfElement -> Integer or false
;; Finds the data element for the given name and returns the data if found, false otherwise.


(define (find-name-element name element)
  (if (string=? name (element-name element))
    (element-data element)
    (find-name-list-of-element name (element-sub-elements element))))


(define (find-name-list-of-element name list-of-element)
  (cond [(empty? list-of-element) false]
        [else
          (if (not (false? (find-name-element name (first list-of-element))))
            (find-name-element name (first list-of-element))
            (find-name-list-of-element name (rest list-of-element)))]))


;; Tests

(check-expect (find-name-list-of-element "F1" empty) false)
(check-expect (find-name-element "F1" F2) false)
(check-expect (find-name-element "F1" F1) 1)
(check-expect (find-name-element "D4" D4) 0)
(check-expect (find-name-list-of-element "F3" (list F1 F2)) false)
(check-expect (find-name-list-of-element "F1" (list F1 F2)) 1)
(check-expect (find-name-list-of-element "F2" (list F1 F2)) 2)
(check-expect (find-name-element "F1" D4) 1)
(check-expect (find-name-element "F2" D4) 2)
(check-expect (find-name-element "F2" D6) 2)
(check-expect (find-name-element "F3" D6) 3)


;; PROBLEM
;; =======
;;
;; Design a function that consumes Element and produces a rendering of the tree. For example:
;;
;; (render-tree D6) should produce something like the following.
;; (image removed)
;;
;; HINTS:
;;   - This function is not very different than the first two functions above.
;;   - Keep it simple! Start with a not very fancy rendering like the one above.
;;     Once that works you can make it more elaborate if you want to.
;;   - And... be sure to USE the recipe. Not just follow it, but let it help you.
;;     For example, work out a number of examples BEFORE you try to code the function.

;; Constants

(define TEXT-SIZE 18)
(define TEXT-COLOR "white")

;; Element -> Image
;; ListOfElement -> Image
;; Produces the tree image of the provided element

(define (render-tree-element element)
  (above (text (element-name element) TEXT-SIZE TEXT-COLOR)
         (render-tree-list-of-element (element-sub-elements element))))

(define (render-tree-list-of-element list-of-element)
  (cond [(empty? list-of-element) empty-image]
        [else
          (beside (render-tree-element (first list-of-element))
                  (render-tree-list-of-element (rest list-of-element)))]))


;; Tests

(check-expect (render-tree-list-of-element empty) empty-image)
(check-expect (render-tree-element F1)
              (above (text (element-name F1) TEXT-SIZE TEXT-COLOR) empty-image))
(check-expect (render-tree-list-of-element (list F1 F2))
              (beside (text (element-name F1) TEXT-SIZE TEXT-COLOR)
                      (text (element-name F2) TEXT-SIZE TEXT-COLOR)))
(check-expect (render-tree-element D4)
              (above (text (element-name D4) TEXT-SIZE TEXT-COLOR)
                     (render-tree-list-of-element (list F1 F2))))
(check-expect (render-tree-element D6)
              (above (text (element-name D6) TEXT-SIZE TEXT-COLOR)
                     (beside (render-tree-element D4) (render-tree-element D5))))
