#lang htdp/asl

;; Problem:
;;
;; Starting with the following data definition for a binary tree (not a binary search tree)
;; design a tail-recursive function called contains? that consumes a key and a binary tree
;; and produces true if the tree contains the key.


(define-struct node (key value left right))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))


;; Integer -> Boolean
;; Produces true if the tree contains the key, false otherwise


(define (contains? key tree)
  ; rem-tree is (listof Node); represents a worklist accumulator
  (local
    [(define (helper-tree tree rem-tree)
       (cond [(false? tree) (helper-rem-tree rem-tree)]
             [else
               (if (= (node-key tree) key)
                 true
                 (helper-tree (node-left tree)
                              (cons (node-right tree)
                                    rem-tree)))]))

     (define (helper-rem-tree rem-tree)
       (if (empty? rem-tree)
         false
         (helper-tree (first rem-tree) (rest rem-tree))))]

    (helper-tree tree empty)))


(check-expect (contains? 1 BT1) false)
(check-expect (contains? 1 BT2) true)
(check-expect (contains? 3 BT2) false)
(check-expect (contains? 7 BT2) true)
