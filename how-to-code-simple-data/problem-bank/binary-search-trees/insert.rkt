#lang htdp/bsl


#| PROBLEM:
 |
 | Design a function that consumes an Integer, String and BST, and adds a node
 | that has the given key and value to the tree. The node should be inserted in
 | the proper place in the tree. The function can assume there is not already
 | an entry for that number in the tree. The function should produce the new BST.
 |
 | Do not worry about keeping the tree balanced. We will come back to this later. |#


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

;; Integer String BST -> BST
;; Inserts a node with given key and value in the proper place in BST
;; ASSUMPTION: Given key is not present in the tree

(define (insert-node num str tree)
  (cond [(false? tree) (make-node num str false false)]
        [else
         (if (< num (node-key tree))
              (make-node
                (node-key tree) (node-val tree)
                (insert-node num str (node-left tree)) (node-right tree))
              (make-node
                (node-key tree) (node-val tree)
                (node-left tree)
                (insert-node num str (node-right tree))))]))


;; Tests

(check-expect (insert-node 1 "abc" BST0) BST1)
(check-expect (insert-node 6 "test" BST4)
              (make-node 4 "dcj" false
                         (make-node 7 "ruf"
                                    (make-node 6 "test" false false) false)))
(check-expect (insert-node 2 "test" BST3)
              (make-node 3 "ilk" (make-node 1 "abc" false
                                            (make-node 2 "test" false false)) BST4))
