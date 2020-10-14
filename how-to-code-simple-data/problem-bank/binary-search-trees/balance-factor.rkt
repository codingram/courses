#lang htdp/bsl

#| PROBLEM:
 |
 | As discussed in lecture, for optimal lookup time we want a BST to be balanced.
 | The oldest approach to this is called AVL self-balancing trees and was invented in 1962.
 | The remainder of this problem set is based on AVL trees.
 |
 | An individual node is balanced when the height of its left and right branches differ
 | by no more than 1. A tree is balanced when all its nodes are balanced.
 |
 | a) Design the function balance-factor that consumes a node and produces its balance factor,
 | which is defined as the height of its left child minus the height of its right child.
 |
 | b) Use your function in part a) to design the function balanced?, which consumes a BST and
 | produces true if the tree is balanced.
 |
 | Once you have the function, use it to compare what happens with the following two sequences
 | of insertions:
 |
 |
 | (insert 4 "a"
 |         (insert 5 "a"
 |                 (insert 6 "a"
 |                         (insert 7 "a"
 |                                 (insert 8 "a" false)))))
 |
 |
 | (insert 4 "a"
 |         (insert 5 "a"
 |                 (insert 8 "a"
 |                         (insert 7 "a"
 |                                 (insert 6 "a" false)))))  |#


; Data definitions:

(define-struct node (key val left right))
; A BST (Binary Search Tree) is one of:
;  - false
;  - (make-node Integer String BST BST)
; interp. false means no BST, or empty BST
;         key is the node key
;         val is the node val
;         left and right are left and right subtrees
; INVARIANT: for a given node:
;     key is > all keys in its left child
;     key is < all keys in its right child
;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

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
;  - self reference: (node-l t) has type BST
;  - self reference: (node-r t) has type BST
