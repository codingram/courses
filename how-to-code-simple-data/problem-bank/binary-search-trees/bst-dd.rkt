#lang htdp/bsl

#| PROBLEM:
 |
 | Design a data definition to represent binary search trees. As a reminder,
 | here is one example BST: |#


(define-struct node (key value left right))
;; Node is (make-node Natural String BST BST)
;; BST (Binary Search Tree) is one of:
;; - false
;; - (make-node Natural String BST BST)
;; interp. false means no BST, or empty BST
;;         key and value is the node key and node value
;;         left and right are left and right subtree from node
;; INVARIANT: for a given node:
;;   key > all keys in its left child
;;   key < all keys in its right child
;;   the same key never appears twice in the tree

;; Examples:
(define BST0 empty)
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
