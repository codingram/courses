#lang htdp/bsl


#| Consider the following data definition for representing an arbitrary number of user
 | accounts. |#


(define-struct account (num name))
; Accounts is one of:
;  - empty
;  - (cons (make-account Natural String) Accounts)
; interp. a list of accounts, where each
;           num  is an account number
;           name is the person's first name
#;
(define (fn-for-accounts accs)
  (cond [(empty? accs) (...)]
        [else
         (... (account-num  (first accs)) ;Natural
              (account-name (first accs)) ;String
              (fn-for-accounts (rest accs)))]))



#| PROBLEM:
 |
 | Complete the design of the lookup-name function below. Note that because this is a search function
 | it will sometimes 'fail'. This happens if it is called with an account number that does not exist
 | in the accounts list it is provided. If this happens the function should produce false. The signature
 | for such a function is written in a special way as shown below. |#


; Accounts Natural -> String or false
; Try to find account with given number in accounts. If found produce name, otherwise produce false.

(define (lookup-acc accs num)
  (cond [(empty? accs) false]
        [else
         (if (= (account-num  (first accs)) num)
           (account-name (first accs))
           (lookup-acc (rest accs) num))]))


; Tests

(define A1 (make-account 1 "Raj"))
(define A2 (make-account 5 "Soham"))
(define A3 (make-account 7 "Ana"))
(define A4 (make-account 9 "Rohan"))

(define ACS0 empty)
(define ACS1 (list A1 A2 A3 A4))

(check-expect (lookup-acc ACS0 1) false)
(check-expect (lookup-acc ACS1 5) "Soham")
(check-expect (lookup-acc ACS1 2) false)
(check-expect (lookup-acc ACS1 7) "Ana")
(check-expect (lookup-acc ACS1 9) "Rohan")
