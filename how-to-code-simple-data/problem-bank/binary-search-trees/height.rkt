#lang htdp/bsl


#| PROBLEM:
 |
 | Design the function height, that consumes a BST and produces its height. Note that
 | the height of a BST is one plus the height of its highest child. You will want to
 | use the BSL max function in your solution. The height of a false tree is 0. The
 | height of (make-node 1 "a" false false) is 1. |#


;; Data definitions:

(define-struct node (key val left right))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         left and right are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its left child
;;     key is < all keys in its right child
;;     the same key never appears twice in the tree

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

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Function

;; BSL -> Natural
;; Produces the number of levels in a tree

(define (height tree)
  (cond [(false? tree) 0]
        [else
         (+ 1 (max (height (node-left tree))
                   (height (node-right tree))))]))


;; Tests

(check-expect (height BST0) 0)
(check-expect (height BST1) 1)
(check-expect (height BST4) 2)
(check-expect (height BST3) 3)
(check-expect (height BST42) 3)
(check-expect (height BST10) 4)
