#lang htdp/isl

(require 2htdp/image)

;; fold-functions-starter.rkt

;; At this point in the course, the type (listof X) means:

;; ListOfX is one of:
;; - empty
;; - (cons X ListOfX)
;; interp. a list of X

;; and the template for (listof X) is:
#;
(define (fn-for-lox lox)
  (cond [(empty? lox) (...)]
        [else
         (... (first lox)
              (fn-for-lox (rest lox)))]))


;; PROBLEM:
;;
;; Design an abstract fold function for (listof X).

;; Explanation:
;; First, we know that lox is going to be of type (listof X), X is a type parameter.
;; Now, the given func takes X as the first argument which is by inferring the
;; ... (func (first lox)... line. Now, we know that the recursive call will produce the
;; value which is of the same type as that of 'base' if the (rest lox) is empty, otherwise
;; it will produce whatever the function 'func' is going to return. We can deduce that the
;; function 'fold' is not necessary to return a value of type X, so let us assume that it is
;; of type Y. Now, if the function 'fold' returns a value of type Y, then that should be the
;; return type for the user provided function 'func' and also the 'base' parameter and thus,
;; the second argument to the user provided function 'func' should also be of type Y.

;; (X Y -> Y) Y (listof X) -> Y
;; The abstract fold function for (listof X)
(define (fold func base lox)
  (cond [(empty? lox) base]
        [else
         (func (first lox)
               (fold func base (rest lox)))]))

(check-expect (fold + 0 '(1 2 3)) 6)
(check-expect (fold * 1 '(1 2 3)) 6)
(check-expect (fold string-append "" '("a" "bc")) "abc")

(define (test-func str2 base)
  (+ base (string-length str2)))
;; Test to show that the fold function does not return a value of the same type as that of the
;; element of the given list.
(check-expect (fold test-func 0 '("a" "bc" "abc")) 6)


;; PROBLEM:
;;
;; Complete the function definition for sum using fold.


;; (listof Number) -> Number
;; add up all numbers in list
(check-expect (sum empty) 0)
(check-expect (sum (list 2 3 4)) 9)

;; (define (sum lon) 0) ;stub

(define (sum lon)
  (fold + 0 lon))


;; PROBLEM:
;;
;; Complete the function definition for juxtapose using foldr.

;; (listof Image) -> Image
;; juxtapose all images beside each other
(check-expect (juxtapose empty) (square 0 "solid" "white"))
(check-expect (juxtapose (list (triangle 6 "solid" "yellow")
                               (square 10 "solid" "blue")))
              (beside (triangle 6 "solid" "yellow")
                      (square 10 "solid" "blue")
                      (square 0 "solid" "white")))

;; (define (juxtapose loi) (square 0 "solid" "white")) ;stub

(define (juxtapose loi)
  (fold beside empty-image loi))


;; PROBLEM:
;;
;; Complete the function definition for copy-list using foldr.

;; (listof X) -> (listof X)
;; produce copy of list
(check-expect (copy-list empty) empty)
(check-expect (copy-list (list 1 2 3)) (list 1 2 3))

;; (define (copy-list lox) empty) ;stub

(define (copy-list lox) (fold cons empty lox))

;; ======================


;; PROBLEM:
;;
;; Design an abstract fold function for Element (and (listof Element)).



(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))
#;
(define (fn-for-element e)
  (local [(define (fn-for-element e)
            (... (elt-name e)    ;String
                 (elt-data e)    ;Integer
                 (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)
            (cond [(empty? loe) (...)]
                  [else
                   (... (fn-for-element (first loe))
                        (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))


;; Mutually recursive abstract function
;; Assuming that the two local functions are producing the return type as X and Y,
;; we are able to deduce the type signature for the abstract function using the two
;; type parameters.

;; (String Integer Y -> X) (X Y -> Y) Y Element -> X
;; Abstract fold function for the Element
(check-expect (letrec ([func1 (lambda (n d loe) (cons n loe))])
                (fold-element func1 append empty D6))
              '("D6" "D4" "F1" "F2" "D5" "F3"))

(define (fold-element func1 func2 base e)
  (local [(define (fn-for-element e)  ; -> X
            (func1 (elt-name e)    ;String
                   (elt-data e)    ;Integer
                   (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)  ; -> Y
            (cond [(empty? loe) base]
                  [else
                   (func2 (fn-for-element (first loe))
                          (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))



;; PROBLEM
;;
;; Complete the design of sum-data that consumes Element and produces
;; the sum of all the data in the element and its subs

;; Element -> Integer
;; produce the sum of all the data in element (and its subs)
(check-expect (sum-data F1) 1)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) (+ 1 2))
(check-expect (sum-data D6) (+ 1 2 3))

;; (define (sum-data e) 0) ;stub

(define (sum-data e)
  (letrec ([sum (lambda (name data total) (+ data total))])
    (fold-element sum + 0 e)))

;; PROBLEM
;;
;; Complete the design of all-names that consumes Element and produces a list of the
;; names of all the elements in the tree.

;; Element       -> ListOfString
;; produce list of the names of all the elements in the tree
(check-expect (all-names F1) (list "F1"))
(check-expect (all-names D5) (list "D5" "F3"))
(check-expect (all-names D4) (list "D4" "F1" "F2"))
(check-expect (all-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))

;; (define (all-names e) empty) ;stub

(define (all-names e)
  (letrec ([append-name (lambda (name data los) (cons name los))])
    (fold-element append-name append empty e)))


;; PROBLEM
;;
;; If the tree is very large, then fold-element is not a good way to implement the find
;; function from last week.  Why? If you aren't sure then discover the answer by implementing
;; find using fold-element and then step the two versions with different arguments.

(check-expect (find-name "F1" F2) false)
(check-expect (find-name "F1" F1) true)
(check-expect (find-name "D4" D4) true)
(check-expect (find-name "F1" D4) true)
(check-expect (find-name "F2" D4) true)
(check-expect (find-name "F2" D6) true)
(check-expect (find-name "F3" D6) true)


;; Explanation:
;; Short answer: We won't get the short circuit behaviour we got from the last weeks
;; function. It means if the tree is large but the element we're trying to find is the first
;; element, it will go to the leaves of the tree and start comparing the elements from that
;; part all the way to the top while the answer was sitting right there.
;;
;; Long answer: Assume the case (find-name "D6" D6)
;; Using the last week function, the first thing we would do is to compare the current node,
;; if they are equal then we got we want and if not, then keep looking down the branches.
;; In the case of using the abstract function, we would not be able to compare the first
;; node first as the call to the compare function requires the third argument which is the
;; answer for the compare to the rest of the nodes. This means we will not even start
;; comparing the node names until we are right at the bottom and then only we will start
;; comparing the node names for each and every node. Even in the worst case scenario where
;; the node we want to find is right at the bottom, there won't be any short circuit
;; behaviour.

(define (find-name name element)
  (local
    [(define (match-data n d lret)
     (if (string=? name n)
       true
       lret))
     ; Raco complains when passing 'or' directly in the fold-element function
     (define (or2 first second)
       (or first second))]
    (fold-element match-data or2 false element)))
