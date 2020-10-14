#lang htdp/bsl

; Consider the following data definition for a binary search tree:


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
 | Complete the design of the lookup-key function below. Note that because this is a search function
 | it will sometimes 'fail'. This happens if it is called with an key that does not exist in the BST
 | it is provided. If this happens the function should produce false. The signature for such a function
 | is written in a special way as shown below. |#


; BST Natural -> String or false
; Try to find node with given key, if found produce value; if not found produce false.

(define (lookup-key tree key)
  (cond [(false? tree) false]
        [else
          (cond [(= (node-key tree) key)
                 (node-value tree)]
                [(> (node-key tree) key)
                 (lookup-key (node-left tree) key)]
                [(< (node-key tree) key)
                 (lookup-key (node-right tree) key)])]))



; Tests

(check-expect (lookup-key BST0 1) false)
(check-expect (lookup-key BST1 1) "a")
(check-expect (lookup-key BST20 15) false)
(check-expect (lookup-key BST20 22) false)
(check-expect (lookup-key BST10 1) "a")  ; Left Left
(check-expect (lookup-key BST10 6) "d")  ; Left Right
(check-expect (lookup-key BST10 14) "f") ; Right Left
(check-expect (lookup-key BST10 27) "h") ; Right Right
