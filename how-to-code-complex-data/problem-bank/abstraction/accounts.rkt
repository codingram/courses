#lang htdp/isl


(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


;; PROBLEM 1:
;;
;; Design an abstract function (including signature, purpose, and tests)
;; to simplify the remove-debtors and remove-profs functions defined below.
;;
;; Now re-define the original remove-debtors and remove-profs functions
;; to use your abstract function. Remember, the signature and tests should
;; not change from the original functions.


;; =======================
;; Abstract function:

;; (Accounts -> Boolean) Accounts -> Accounts
;; Remember: Accounts type is either false or (make-node ...)
;; Abstract function to remove accounts if the predicate function returns true

(define (remove-accounts pred act)
  (cond [(false? act) false]
        [else
          (if (pred act)
            (join (remove-accounts pred (node-l act))
                  (remove-accounts pred (node-r act)))
            (make-node (node-id act)
                       (node-name act)
                       (node-bal act)
                       (remove-accounts pred (node-l act))
                       (remove-accounts pred (node-r act))))]))


;; Accounts -> Accounts
;; remove all accounts with a negative balance

(define (remove-debtors act)
  (letrec ([debt? (lambda (act) (negative? (node-bal act)))])
    (remove-accounts debt? act)))


;; Accounts -> Accounts
;; Remove all professors' accounts.

(define (remove-profs act)
  (letrec ([is-prof? (lambda (act) (has-prefix? "Prof." (node-name act)))])
    (remove-accounts is-prof? act)))


;; String String -> Boolean
;; Determine whether pre is a prefix of str.

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))


;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2)
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


;; ========
;; Tests:

(check-expect (letrec ([always-true (lambda (act) true)])
                (remove-accounts always-true false))
              false)
(check-expect (letrec ([always-true (lambda (act) true)])
                (remove-accounts always-true ACT1))
              false)
(check-expect (letrec ([always-false (lambda (act) false)])
                (remove-accounts always-false ACT1))
              ACT1)
(check-expect (letrec ([debt? (lambda (act) (negative? (node-bal act)))])
                (remove-accounts debt? ACT4))
              (make-node 7 "Mr. Natural" 13 false false))


(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false))
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors
               (make-node 4 "Mrs. Doubtfire" -3
                          false
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))

(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false))
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs
               (make-node 97 "Prof. X" 7
                          false
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))


(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)


(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4)
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42)
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))


;; ==============
;; PROBLEM 2:
;;
;; Using your new abstract function, design a function that removes from a given
;; BST any account where the name of the account holder has an odd number of
;; characters.  Call it remove-odd-characters.

;; Accounts -> Accounts
;; Removes all the account where the name of the account holder has an odd number of characters

(define (remove-odd-characters act)
  (letrec ([has-odd-chars? (lambda (act) (odd? (string-length (node-name act))))])
    (remove-accounts has-odd-chars? act)))

;; Tests:

(check-expect (remove-odd-characters false) false)
(check-expect (remove-odd-characters ACT1) ACT1)
(check-expect (remove-odd-characters ACT4) (make-node 4 "Mrs. Doubtfire" -3 false false))


;; =============
;; Problem 3:
;;
;; Design an abstract fold function for Accounts called fold-act.
;;
;; Use fold-act to design a function called charge-fee that decrements
;; the balance of every account in a given collection by the monthly fee of 3 CAD.

;; (Natural String Integer X X -> X) X Accounts -> X
;; Abstract fold function for Accounts

(define (fold-act func base act)
  (cond [(false? act) base]
        [else
         (func (node-id act)
               (node-name act)
               (node-bal act)
               (fold-act func base (node-l act))
               (fold-act func base (node-r act)))]))


;; Accounts -> Accounts
;; Decrease the balance of every account in a given collection by the monthly fee of 3 CAD

(define (charge-fee act)
  (local
    [(define (decrease-bal id name bal l r)
       (make-node id name (- bal 3) l r))]
    (fold-act decrease-bal false act)))


;; Tests:

(check-expect (charge-fee false) false)
(check-expect (charge-fee ACT1) (make-node 1 "Mr. Rogers" 19 false false))

;; =============
;; PROBLEM 4:
;;
;; Suppose you needed to design a function to look up an account based on its ID.
;; Would it be better to design the function using fold-act, or to design the
;; function using the fn-for-acts template?  Briefly justify your answer.

;; fn-for-acts would be better. The lookup starts from the root of the tree all the way to the top.
;; Even if the node we are looking for is right at the beginning, the function will traverse the tree
;; in every branches and only then it will check the first node.
