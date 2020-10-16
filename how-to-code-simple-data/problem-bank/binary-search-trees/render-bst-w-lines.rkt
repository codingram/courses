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
         (above (render-key-val (node-key tree) (node-value tree)
                                (+ (image-width (render-tree (node-left tree)))
                                   (image-width (render-tree (node-right tree)))))
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
      (add-line (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" BG-COLOR)
                (/ (+ lw rw) 2) 0
                (/ lw 2) (/ (+ lw rw) 4) LINE-COLOR)
      (add-line (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" BG-COLOR)
                (/ (+ lw rw) 2) 0
                (+ lw (/ rw 2)) (/ (+ lw rw) 4) LINE-COLOR)))


; Natural Natural String String -> Image
; Decides from either both lines or none lines and calls the appropriate function

(define (none-or-both lw rw ls rs)
  (if (and (string=? ls SENTINAL) (string=? rs SENTINAL))
      (render-both-lines lw rw)
      (render-no-lines lw rw)))

; Natural Natural -> Image
; Produces an image with both lines

(define (render-both-lines lw rw)
  (add-line (add-line (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" BG-COLOR)
                      (/ (+ lw rw) 2) 0
                      (/ lw 2) (/ (+ lw rw) 4)
                      LINE-COLOR)
            (/ (+ lw rw) 2) 0
            (+ lw (/ rw 2)) (/ (+ lw rw) 4)
            LINE-COLOR))

; Natural Natural -> Image
; Produces a blank image without any lines

(define (render-no-lines lw rw)
  (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" BG-COLOR))

; Natural String Natural -> Image
; Helper function to produce the key value pair image

(define (render-key-val key val tw)
  (place-image (text (string-append (number->string key) SEP val)
                     TEXT-SIZE TEXT-COLOR)
               (/ tw 2) (/ TEXT-SIZE 2)
               (rectangle tw TEXT-SIZE "solid" BG-COLOR)))

; Tests 
; TODO Add test cases

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100
  (make-node 100 "large" BST10 false))
