#lang htdp/bsl


; PROBLEM:
;
; Design the function count-nodes, which consumes BST and produces a
; natural number which is the total number of nodes in the BST. An
; empty tree (false) has 0 nodes.


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
;     key is > all keys in its left  child
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
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-left t))
              (fn-for-bst (node-right t)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic-distinct: false
;  - compound: (make-node Integer String BST BST)
;  - self reference: (node-left t) has type BST
;  - self reference: (node-right t) has type BST


; Function

; BST -> Natural
; Produces the number of nodes present in the given tree

(define (count-nodes tree)
  (cond [(false? tree) 0]
        [else
         (+ 1
            (count-nodes (node-left tree))
            (count-nodes (node-right tree)))]))


; Tests

(check-expect (count-nodes BST0) 0)
(check-expect (count-nodes BST1) 1)
(check-expect (count-nodes BST3) 4)
(check-expect (count-nodes BST4) 2)
(check-expect (count-nodes BST42) 3)
(check-expect (count-nodes BST10) 8)
