#lang htdp/bsl


(require 2htdp/image)

#| PROBLEM:
 |
 | Given the following data definition for a binary search tree,
 | design a function that consumes a bst and produces a SIMPLE
 | rendering of that bst including lines between nodes and their
 | subnodes.
 |
 | To help you get started, we've added some sketches below of
 | one way you could include lines to a bst. |#

; Constants

(define BG-COLOR "black")
(define TEXT-SIZE  14)
(define TEXT-COLOR "white")
(define SEP ": ")
(define BLANK (rectangle 40 1 "solid" "black"))
(define LINE-COLOR "white")
(define SENTINAL "line")


; Data definitions:

(define-struct node (key value left right))
; Node is (make-node Natural String BST BST)
; BST (Binary Search Tree) is one of:
; - false
; - (make-node Natural String BST BST)
; interp. false means no BST, or empty BST
;         key and value is the node key and node value
;         left and right are left and right subtree from node
; INVARIANT: for a given node:
;   key > all keys in its left child
;   key < all keys in its right child
;   the same key never appears twice in the tree

#;
(define (fn-for-bst tree)
  (cond [(false? tree) (...)]
        [else
         (... (node-key tree)
              (node-val tree)
              (fn-for-bst (node-left tree))
              (fn-for-bst (node-right tree)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic-distinct: false
;  - compound: (make-node Integer String BST BST)
;  - self reference: (node-left tree) has type BST
;  - self reference: (node-right tree) has type BST


; Functions:

; BST -> Image
; Produces the SIMPLE image of the given tree with lines

(define (render-tree tree)
  (cond [(false? tree) BLANK]
        [else
         (above (render-key-val (node-key tree) (node-value tree))
                (render-lines (image-width (render-tree (node-left tree)))
                              (image-width (render-tree (node-right tree)))
                              (which-sentinal (node-left tree))
                              (which-sentinal (node-right tree)))
                (beside/align "top"
                              (render-tree (node-left tree))
                              (render-tree (node-right tree))))]))

; false or BST -> String
; Produces either "line" or "noline" depending on whether the input
; is false or BST
; Helper function for render-tree

(define (which-sentinal tree)
  (if (false? tree)
      "noline"
      "line"))

; Natural Natural String String -> Image
; Produces an image with lines if it doesn't lead to false BST

(define (render-lines lw rw ls rs)
  (if (one-of? ls rs)
      (one-of-lines lw rw ls rs)
      (none-or-both lw rw ls rs)))

; String String -> Boolean
; Produces true only if one of the input value equals SENTINAL

(define (one-of? ls rs)
  (or
   (and (string=? ls SENTINAL) (not (string=? rs SENTINAL)))
   (and (string=? rs SENTINAL) (not (string=? ls SENTINAL)))))

; Natural Natural String String -> Image
; Helper function when one of the lines needs to be drawn

(define (one-of-lines lw rw ls rs)
  (if (string=? ls SENTINAL)
      (add-line (get-rect lw rw)
                (/ (+ lw rw) 2) 0
                (/ lw 2) (/ (+ lw rw) 4) LINE-COLOR)
      (add-line (get-rect lw rw)
                (/ (+ lw rw) 2) 0
                (+ lw (/ rw 2)) (/ (+ lw rw) 4) LINE-COLOR)))


; Natural Natural String String -> Image
; Decides from either both lines or none lines and calls the appropriate function

(define (none-or-both lw rw ls rs)
  (if (and (string=? ls SENTINAL) (string=? rs SENTINAL))
      (render-both-lines lw rw)
      (get-rect lw rw)))

; Natural Natural -> Image
; Produces an image with both lines

(define (render-both-lines lw rw)
  (add-line (add-line (get-rect lw rw)
                      (/ (+ lw rw) 2) 0
                      (/ lw 2) (/ (+ lw rw) 4)
                      LINE-COLOR)
            (/ (+ lw rw) 2) 0
            (+ lw (/ rw 2)) (/ (+ lw rw) 4)
            LINE-COLOR))

; Natural String Natural -> Image
; Helper function to produce the key value pair image

(define (render-key-val key val)
  (text (string-append (number->string key) SEP val)
        TEXT-SIZE TEXT-COLOR))

; Natural Natural -> Image
; Helper function to create the rectangle background

(define (get-rect lw rw)
  (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" BG-COLOR))


; Tests

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST2 (make-node 7 "ruf" BST1 false))
(define BST3 (make-node 4 "dcj" false BST2))
(define BST4 (make-node 9 "efg" BST2 BST3))
(define BST5 (make-node 11 "ily" BST2 false))
(define BST6 (make-node 12 "ran" (make-node 13 "hel" BST3 BST5) BST4))
(define BST7 (make-node 10 "fnl" BST4 BST6))


; Tests for helper functions

(check-expect (which-sentinal BST0) "noline")
(check-expect (which-sentinal BST1) "line")

(check-expect (one-of? "line" "line") false)
(check-expect (one-of? "noline" "line") true)
(check-expect (one-of? "line" "noline") true)
(check-expect (one-of? "noline" "noline") false)

(check-expect (render-key-val 1 "a")
              (text (string-append "1" SEP "a") TEXT-SIZE TEXT-COLOR))

(check-expect (get-rect 40 60) (rectangle 100 25 "solid" BG-COLOR))
(check-expect (get-rect 0 100) (rectangle 100 25 "solid" BG-COLOR))

; If all the checks passed for helper functions, we can use them in our
; other test cases

(check-expect (render-both-lines 40 60)
              (add-line (add-line (get-rect 40 60) 50 0 20 25 LINE-COLOR)
                        50 0 70 25 LINE-COLOR))

(check-expect (none-or-both 40 60 "noline" "noline") (get-rect 40 60))
(check-expect (none-or-both 40 60 "line" "line")
              (add-line (add-line (get-rect 40 60) 50 0 20 25 LINE-COLOR)
                        50 0 70 25 LINE-COLOR))

(check-expect (one-of-lines 40 60 "line" "noline")
              (add-line (get-rect 40 60) 50 0 20 25 LINE-COLOR))
(check-expect (one-of-lines 40 60 "noline" "line")
              (add-line (get-rect 40 60) 50 0 70 25 LINE-COLOR))

; For render-lines and render-tree we need to only check whether the function calls 
; the correct function according to the input this function contains only calls to
; already tested functions.

(check-expect (render-lines 40 60 "line" "noline") (one-of-lines 40 60 "line" "noline"))
(check-expect (render-lines 40 60 "noline" "line") (one-of-lines 40 60 "noline" "line"))
(check-expect (render-lines 40 60 "line" "line") (none-or-both 40 60 "line" "line"))
(check-expect (render-lines 40 60 "noline" "noline") (none-or-both 40 60 "noline" "noline"))

(check-expect (render-tree BST0) BLANK)
(check-expect (render-tree BST1)
              (above (render-key-val 1 "abc")
                     (render-lines (image-width (render-tree false))
                                   (image-width (render-tree false))
                                   "noline" "noline")
                     (beside/align "top"
                                   (render-tree false)
                                   (render-tree false))))
