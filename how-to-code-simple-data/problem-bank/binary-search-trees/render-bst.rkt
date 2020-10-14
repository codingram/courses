#lang htdp/bsl


(require 2htdp/image)

; Consider the following data definition for a binary search tree:

; Constants:

(define TEXT-SIZE 16)
(define TEXT-COLOR "white")
(define SEP ": ")
(define VSPACE (rectangle 1 20 "solid" "black"))
(define HSPACE (rectangle 20 1 "solid" "black"))
(define BLANK (rectangle 20 20 "solid" "black"))

; Data definitions:
; From ./bsl-dd.rkt

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

; Examples:
(define BST0 false)
(define BST1 (make-node 1 "a" false false))
(define BST3 (make-node 3 "b" false false))
(define BST8 (make-node 8 "c" false false))
(define BST6 (make-node 6 "d" BST3 BST8))
(define BST2 (make-node 2 "e" BST1 BST6))

(define BST14 (make-node 14 "f" false false))
(define BST20 (make-node 20 "g" false false))
(define BST27 (make-node 27 "h" BST20 false))
(define BST17 (make-node 17 "i" BST14 BST27))

(define BST10 (make-node 10 "j" BST2 BST17))

(define (fn-for-bst tree)
  (cond [(false? tree) (...)]
        [else
         (... (node-key tree)
              (node-value tree)
              (fn-for-tree (node-left tree))
              (fn-for-tree (node-right tree)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic-distinct: false
;  - compound: (make-node Integer String BST BST)
;  - self reference: (node-left t) has type BST
;  - self reference: (node-right t) has type BST

; Functions:

#| PROBLEM:
 |
 | Design a function that consumes a bst and produces a SIMPLE
 | rendering of that bst. Emphasis on SIMPLE. You might want to
 | skip the lines for example. |#


; BST -> Image
; Produces a SIMPLE image for the given tree

(define (render-bst tree)
  (cond [(false? tree) BLANK]
        [else
         (above (text
                  (string-append
                    (number->string (node-key tree))
                    SEP
                    (node-value tree))
                  TEXT-SIZE TEXT-COLOR)
                VSPACE
                (beside
                 (render-bst (node-left tree))
                 HSPACE
                 (render-bst (node-right tree))))]))


; Tests

(check-expect (render-bst BST0) BLANK)
(check-expect (render-bst BST1)
              (above  (text (string-append "1" SEP "a")
                            TEXT-SIZE TEXT-COLOR)
                      VSPACE
                      (beside (render-bst false)
                              HSPACE
                              (render-bst false))))
(check-expect (render-bst BST27)
              (above (text (string-append "27" SEP "h")
                           TEXT-SIZE TEXT-COLOR)
                     VSPACE
                     (beside (render-bst BST20)
                             HSPACE
                             (render-bst false))))
(check-expect (render-bst BST6)
              (above (text (string-append "6" SEP "d")
                           TEXT-SIZE TEXT-COLOR)
                     VSPACE
                     (beside (render-bst BST3)
                             HSPACE
                             (render-bst BST8))))

; Demo for the program
(render-bst BST10)
