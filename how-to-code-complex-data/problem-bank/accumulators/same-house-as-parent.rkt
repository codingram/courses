#lang htdp/asl


;; PROBLEM:
;;
;; In the Harry Potter movies, it is very important which of the four houses a
;; wizard is placed in when they are at Hogwarts. This is so important that in
;; most families multiple generations of wizards are all placed in the same family.
;;
;; Design a representation of wizard family trees that includes, for each wizard,
;; their name, the house they were placed in at Hogwarts and their children. We
;; encourage you to get real information for wizard families from:
;;    http://harrypotter.wikia.com/wiki/Main_Page
;;
;; The reason we do this is that designing programs often involves collection
;; domain information from a variety of sources and representing it in the program
;; as constants of some form. So this problem illustrates a fairly common scenario.
;;
;; That said, for reasons having to do entirely with making things fit on the
;; screen in later videos, we are going to use the following wizard family tree,
;; in which wizards and houses both have 1 letter names. (Sigh)


;; --------------------
;; Data definitions:

(define-struct wizard (name house children))
;; Wizard is (make-wizard String String (listof Wizard))
;; interp. wizard with name, house and list of children

(define Wa (make-wizard "A" "S" empty))
(define Wb (make-wizard "B" "G" empty))
(define Wc (make-wizard "C" "R" empty))
(define Wd (make-wizard "D" "H" empty))
(define We (make-wizard "E" "R" empty))
(define Wf (make-wizard "F" "R" (list Wb)))
(define Wg (make-wizard "G" "S" (list Wa)))
(define Wh (make-wizard "H" "S" (list Wc Wd)))
(define Wi (make-wizard "I" "H" empty))
(define Wj (make-wizard "J" "R" (list We Wf Wg)))
(define Wk (make-wizard "K" "G" (list Wh Wi Wj)))


#; template
(define (fn-for-wizard wizard)
  (local
    [(define (helper-wizard wizard)
       (... (wizard-name wizard)
            (wizard-house wizard)
            (helper-children (wizard-children wizard))))
     (define (helper-children children)
       (cond [(empty? children) (...)]
             [else
               (... (helper-wizard (first children))
                    (helper-children (rest children)))]))]
    (helper-wizard wizard)))



;; PROBLEM:
;;
;; Design a function that consumes a wizard and produces the names of every
;; wizard in the tree that was placed in the same house as their immediate
;; parent.


;; Wizard -> (listof String)
;; Produce a list of names of every wizard in the tree that was placed in the same house
;; as their immediate parent.

(define (same-house-names wizard)
  ; parent-house: String; represents the name of the current wizard's immediate parent house
  ; (same-house-names Wj)
  ; (helper-wizard Wj "")  ; No information about the given root wizard's parent house
  ; (helper-wizard We "R")
  ; (helper-wizard Wf "R")
  ; (helper-wizard Wb "R")
  ; (helper-wizard Wg "R")
  ; (helper-wizard Wa "S")
  (local
    [
     ; Wizard -> (listof String)
     (define (helper-wizard wizard parent-house)
       (if (string=? (wizard-house wizard) parent-house)
         (cons (wizard-name wizard)
               (helper-children (wizard-children wizard)
                                (wizard-house wizard)))
         (helper-children (wizard-children wizard)
                          (wizard-house wizard))))
     ; (listof Wizard) -> (listof String)
     (define (helper-children children parent-house)
       (cond [(empty? children) empty]
             [else
               (append
                 (helper-wizard (first children) parent-house)
                 (helper-children (rest children) parent-house))]))]
    (helper-wizard wizard "")))


(check-expect (same-house-names We) empty)
(check-expect (same-house-names Wh) empty)
(check-expect (same-house-names Wg) '("A"))
(check-expect (same-house-names Wk) '("E" "F" "A"))


;; PROBLEM:
;;
;; Design a function that consumes a wizard and produces the number of wizards
;; in that tree (including the root). Your function should be tail recursive.


;; Wizard -> Natural
;; Produces the total number of wizards in the given tree (including the root)

(define (total-wizards wizard)
  ; total: Natural; represents the total number of wizards seen so far.
  ; rem-wizard: (listof Wizard); represents the wizards remaining so far.
  ; (total-wizards Wj)
  ; (helper-wizard Wj 0)
  ; (helper-wizard We 1)
  ; (helper-wizard Wf 2)
  ; (helper-wizard Wb 3)
  ; ...
  (local
    [
     ; Wizard -> Natural
     (define (helper-wizard wizard rem-wizard total)
       (helper-children
         ; Appending in this order creates a depth first traversal while appending
         ; in the reverse order (append rem-wizard (wizard-children wizard)) creates
         ; a breadth first traversal!
         ; Appending the children of a node first will make it depth first traversal
         ; as the list represents a worklist accumulator which is saying that these many
         ; nodes are remaining to visit.
         (append
           (wizard-children wizard)
           rem-wizard)
         (add1 total)))
     ; (listof Wizard) -> Natural
     (define (helper-children children total)
       (cond [(empty? children) total]
             [else
               (helper-wizard
                 (first children)
                 (rest children)
                 total)]))]
    (helper-wizard wizard '() 0)))


(check-expect (total-wizards Wi) 1)
(check-expect (total-wizards Wh) 3)
(check-expect (total-wizards Wj) 6)
(check-expect (total-wizards Wk) 11)




;; PROBLEM:
;;
;; Design a new function definition for same-house-as-parent that is tail
;; recursive. You will need a worklist accumulator.


;; Wizard -> (listof String)
;; Another version for same-house-names using tail recursion

(define (same-house-names2 wizard)
  ; rem-wizard: (listof Entry) ; represents a worklist accumulator
  ; names: (listof String) ; represents the function results so far
  ; parent-house: String ; represents the wizard's parent house name
  (local
    [
     ; Entry is (make-entry Wizard String)
     ; interp. a worklist entry consisting of wizard and parent house
     (define-struct entry (wizard parent-house))

     (define (helper-wizard rem-wizard wizard parent-house names)
       (helper-children
         (append (map
                   (lambda (child)
                     (make-entry child (wizard-house wizard)))
                   (wizard-children wizard))
                 rem-wizard)
         (append names
                 (if (string=? (wizard-house wizard) parent-house)
                   (list (wizard-name wizard))
                   empty))))

     (define (helper-children rem-wizard names)
       (cond [(empty? rem-wizard) names]
             [else
               (helper-wizard
                 (rest rem-wizard)
                 (entry-wizard (first rem-wizard))
                 (entry-parent-house (first rem-wizard))
                 names)]))]

    (helper-wizard empty wizard "" empty)))



(check-expect (same-house-names2 We) empty)
(check-expect (same-house-names2 Wh) empty)
(check-expect (same-house-names2 Wg) '("A"))
(check-expect (same-house-names2 Wk) '("E" "F" "A"))
