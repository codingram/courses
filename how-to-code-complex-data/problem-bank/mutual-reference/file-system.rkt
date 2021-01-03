#lang htdp/isl

(require 2htdp/image)

;; =================
;; Data definitions:

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


;; ==========
;; Templates:
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


;; ==========
;; Functions:

;; PROBLEM
;;
;; Design a function that consumes Element and produces the sum of all the file data in
;; the tree.

;; Element -> Integer
;; Produces the sum of file data in the given element (arbitrarily arity tree)


(define (sum-data element)
  (local
    [(define (element-helper element)
       (if (zero? (element-data element))
         (list-of-element-helper (element-sub-elements element))
         (element-data element)))
     (define (list-of-element-helper list-of-element)
       (cond [(empty? list-of-element) 0]
             [else
               (+ (element-helper (first list-of-element))
                  (list-of-element-helper (rest list-of-element)))]))]
    (element-helper element)))


;; =======
;; Tests:

(check-expect (sum-data F1) 1)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) 3)
(check-expect (sum-data D6) 6)


;; PROBLEM
;;
;; Design a function that consumes Element and produces a list of the names of all the elements in
;; the tree.


;; Element -> ListOfString
;; Produces the list of names of all the elements in the tree.


(define (all-names element)
  (local
    [(define (element-helper element)
       (append (list (element-name element))
               (list-of-element-helper (element-sub-elements element))))
     (define (list-of-element-helper list-of-element)
       (cond [(empty? list-of-element) empty]
             [else
               (append (element-helper (first list-of-element))
                       (list-of-element-helper (rest list-of-element)))]))]
    (element-helper element)))


;; =======
;; Tests:

(check-expect (all-names F1) (list "F1"))
(check-expect (all-names D5) (list "D5" "F3"))
(check-expect (all-names D4) (list "D4" "F1" "F2"))
(check-expect (all-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))


;; PROBLEM
;;
;; Design a function that consumes String and Element and looks for a data element with the given
;; name. If it finds that element it produces the data, otherwise it produces false.

;; String Element -> Integer or false
;; Finds the data element for the given name and returns the data if found, false otherwise.


(define (find-name name element)
  (local
    [(define (element-helper name element)
       (if (string=? name (element-name element))
         (element-data element)
         (list-of-element-helper name (element-sub-elements element))))
     (define (list-of-element-helper name list-of-element)
       (cond [(empty? list-of-element) false]
             [else
               (local
                 [(define try (element-helper name (first list-of-element)))]
                 (if (not (false? try))
                   try
                   (list-of-element-helper name (rest list-of-element))))]))]
    (element-helper name element)))


;; =======
;; Tests:

(check-expect (find-name "F1" F2) false)
(check-expect (find-name "F1" F1) 1)
(check-expect (find-name "D4" D4) 0)
(check-expect (find-name "F1" D4) 1)
(check-expect (find-name "F2" D4) 2)
(check-expect (find-name "F2" D6) 2)
(check-expect (find-name "F3" D6) 3)


;; PROBLEM
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

;; ==========
;; Constants:

(define TEXT-SIZE 18)
(define TEXT-COLOR "white")

;; Element -> Image
;; Produces the tree image of the provided element

(define (render-tree element)
  (local
    [(define (element-helper element)
       (above (text (element-name element) TEXT-SIZE TEXT-COLOR)
              (list-of-element-helper (element-sub-elements element))))
     (define (list-of-element-helper list-of-element)
       (cond [(empty? list-of-element) empty-image]
             [else
               (beside (element-helper (first list-of-element))
                       (list-of-element-helper (rest list-of-element)))]))]
    (element-helper element)))


;; ========
;; Tests:

(check-expect (render-tree F1)
              (above (text (element-name F1) TEXT-SIZE TEXT-COLOR) empty-image))
(check-expect (render-tree D4)
              (above (text (element-name D4) TEXT-SIZE TEXT-COLOR)
                     (beside (text (element-name F1) TEXT-SIZE TEXT-COLOR)
                             (text (element-name F2) TEXT-SIZE TEXT-COLOR))))
(check-expect (render-tree D6)
              (above (text (element-name D6) TEXT-SIZE TEXT-COLOR)
                     (beside (render-tree D4) (render-tree D5))))
