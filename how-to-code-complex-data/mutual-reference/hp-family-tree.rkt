#lang htdp/bsl

;; In this problem set you will represent information about descendant family
;; trees from Harry Potter and design functions that operate on those trees.
;;
;; To make your task much easier we suggest two things:
;;   - you only need a DESCENDANT family tree
;;   - read through this entire problem set carefully to see what information
;;     the functions below are going to need. Design your data definitions to
;;     only represent that information.
;;   - you can find all the information you need by looking at the individual
;;     character pages like the one we point you to for Arthur Weasley.


;; PROBLEM 1:
;;
;; Design a data definition that represents a family tree from the Harry Potter
;; wiki, which contains all necessary information for the other problems.  You
;; will use this data definition throughout the rest of the homework.

;; Data definitions
;; ================

(define-struct wizard (name wand patronus children))
;; Wizard is (make-wizard String String String ListOfWizard)
;; interp. A wizard in the descendant family tree
;;         name (String): first name of the wizard
;;         wand (String): material their primary wand is made of ("" if unknown)
;;         patronus (String): guardian name for the wizard ("" if unknown)
;;         children (ListOfWizard): immediate descendants

;; ListOfWizard is one of:
;; - empty
;; - (cons Wizard ListOfWizard)
;; interp. a list of wizards


;; PROBLEM 2:
;;
;; Define a constant named ARTHUR that represents the descendant family tree for
;; Arthur Weasley. You can find all the infomation you need by starting
;; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;;
;; You must include all of Arthur's children and these grandchildren: Lily,
;; Victoire, Albus, James.
;;
;;
;; Note that on the Potter wiki you will find a lot of information. But for some
;; people some of the information may be missing. Enter that information with a
;; special value of "" (the empty string) meaning it is not present. Don't forget
;; this special value when writing your interp.

(define ARTHUR
  (make-wizard
    "Arthur" "" "Weasel"
    (list (make-wizard
            "Bill" "" ""
            (list (make-wizard "Victoire" "" "" empty)
                  (make-wizard "Dominique" "" "" empty)
                  (make-wizard "Louis" "" "" empty)))
          (make-wizard
            "Charlie" "ash" "" empty)
          (make-wizard
            "Percy" "" ""
            (list (make-wizard "Molly" "" "" empty)
                  (make-wizard "Lucy" "" "" empty)))
          (make-wizard
            "Fred" "" "" empty)
          (make-wizard
            "George" "" ""
            (list (make-wizard "Fred" "" "" empty)
                  (make-wizard "Roxanne" "" "" empty)))
          (make-wizard
            "Ron" "ash" "Jack Russell Terrier"
            (list (make-wizard "Rose" "" "" empty)
                  (make-wizard "Hugo" "" "" empty)))
          (make-wizard
            "Ginny" "" "horse"
            (list (make-wizard "James" "" "" empty)
                  (make-wizard "Albus" "" "" empty)
                  (make-wizard "Lily" "" "" empty))))))

;; Templates:
#;
(define (fn-for-wizard wizard)
  (... (wizard-name wizard)
       (wizard-wand wizard)
       (wizard-patronus wizard)
       (fn-for-list-of-wizard (wizard-children wizard))))
#;
(define (fn-for-list-of-wizard list-of-wizard)
  (cond [(empty? list-of-wizard) (...)]
        [else
          (... (fn-for-wizard (first list-of-wizard))
               (fn-for-list-of-wizard (rest list-of-wizard)))]))


;; PROBLEM 3:
;;
;; Design a function that produces a pair list (i.e. list of two-element lists)
;; of every person in the tree and his or her patronus. For example, assuming
;; that HARRY is a tree representing Harry Potter and that he has no children
;; (even though we know he does) the result would be: (list (list "Harry" "Stag")).
;;
;; You must use ARTHUR as one of your examples.

;; ListOfPair is one of:
;; - empty
;; - (cons (list String String) ListOfPair)
(define LOP1 empty)
(define LOP2 (list (list "Harry" "stag") (list "Hermione" "otter")))

;; Wizard -> ListOfPair
;; ListOfWizard -> ListOfPair
;; Produces a list of two element list containing the name and patronus of the
;; given wizard and the children wizards.

(define (patronus-wizard wizard)
  (cons (list (wizard-name wizard)
              (wizard-patronus wizard))
        (patronus-list-of-wizard (wizard-children wizard))))

(define (patronus-list-of-wizard list-of-wizard)
  (cond [(empty? list-of-wizard) empty]
        [else
          (append (patronus-wizard (first list-of-wizard))
                  (patronus-list-of-wizard (rest list-of-wizard)))]))


;; Tests

(check-expect (patronus-list-of-wizard empty) empty)
(check-expect (patronus-wizard (make-wizard "name" "wand" "patronus" empty))
              (list (list "name" "patronus")))
(check-expect (patronus-wizard ARTHUR)
              (list
               (list "Arthur" "Weasel")
               (list "Bill" "")
               (list "Victoire" "")
               (list "Dominique" "")
               (list "Louis" "")
               (list "Charlie" "")
               (list "Percy" "")
               (list "Molly" "")
               (list "Lucy" "")
               (list "Fred" "")
               (list "George" "")
               (list "Fred" "")
               (list "Roxanne" "")
               (list "Ron" "Jack Russell Terrier")
               (list "Rose" "")
               (list "Hugo" "")
               (list "Ginny" "horse")
               (list "James" "")
               (list "Albus" "")
               (list "Lily" "")))


;; PROBLEM 4:
;;
;; Design a function that produces the names of every person in a given tree
;; whose wands are made of a given material.
;;
;; You must use ARTHUR as one of your examples.

;; String Wizard -> ListOfString
;; String ListOfWizard -> ListOfString???
;; Produces a list of names whose wand material matches the given, empty if not found


(define (same-wand-wizard wand wizard)
  (if (string=? wand (wizard-wand wizard))
       (cons (wizard-name wizard)
             (same-wand-list-of-wizard wand (wizard-children wizard)))
       (same-wand-list-of-wizard wand (wizard-children wizard))))

(define (same-wand-list-of-wizard wand list-of-wizard)
  (cond [(empty? list-of-wizard) empty]
        [else
          (append (same-wand-wizard wand (first list-of-wizard))
               (same-wand-list-of-wizard wand (rest list-of-wizard)))]))


;; Tests

(check-expect (same-wand-list-of-wizard "x" empty) empty)
(check-expect (same-wand-wizard "x" (make-wizard "a" "b" "c" empty)) empty)
(check-expect (same-wand-wizard "b" (make-wizard "a" "b" "c" empty)) (list "a"))
(check-expect (same-wand-wizard "ash" ARTHUR) (list "Charlie" "Ron"))
