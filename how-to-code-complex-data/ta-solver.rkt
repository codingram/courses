#lang htdp/asl


;; PROBLEM 1:
;;
;; Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;; whether or not they are a verified user, and follows some number of people.
;;
;; Design a data definition for Chirper, including a template that is tail recursive and avoids
;; cycles.
;;
;; Then design a function called most-followers which determines which user in a Chirper Network is
;; followed by the most people.

(define-struct user (name verified followings))
;; User is (make-chirper (String Boolean (listof User)))
;; interp. a chirper with a name, a note about whether they are verified or not and other
;;         a list of followings

(define U1 (make-user "A" false (list (make-user "B" false empty))))

(define U2
  (shared ([-0- (make-user "A" true (list (make-user "B" false (list -0-))))])
          -0-))

(define U3
  (shared ([-0- (make-user "A" false (list -1- (make-user "D" true (list -2-))))]
           [-1- (make-user "B" true (list (make-user "C" false (list -1-)) -2-))]
           [-2- (make-user "E" false (list (make-user "F" false empty) -0-))])
          -0-))


;; Template:
;; - Structural recursion
;; - Arbitrary arity tree w/ local
;; - Context preserving accumulator which users have we already visited
;; - Tail recursion with worklist accumulator
#;
(define (fn-for-user user)
  ;; visited is (listof string); context preserving accumulator, users already visited
  ;; remaining is (listof User); a worklist accumulator, represents users left to visit
  (local
    [(define (helper-user user visited remaining)
       (if (member (user-name user) visited)
         (helper-user-list remaining visited)
         (helper-user-list (append (user-followings user) remaining)
                           (cons (user-name user) visited))))
     (define (helper-user-list user-list visited)
       (cond [(empty? user-list) (... visited)]
             [else
               (... visited
                    (helper-user (first user-list) visited)
                    (helper-user-list (rest user-list) visited))]))]
    (helper-user user ... ...)))


;; Same solution as of for max-exits-to
(define (most-followers user0)
  ;; remaining is (listof User); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, users already visited
  ;; result is Result; represents the result so far
  (local [
          ;; Result is (make-result-room Room Natural)
          ;; interp. result data comprising of the current user with the most followers
          ;; and the number of times it was total
          (define-struct result (user total))

          ;; User Result Natural -> User
          (define (compare user result nvisits)
            (if (string=? (user-name user) (user-name (result-user result)))
              (make-result (result-user result) (add1 (result-total result)))
              (if (> nvisits (result-total result))
                (make-result user nvisits)
                result)))

          ;; String (listof String) -> Natural
          (define (count-occurence elem lst)
            (foldl
              (lambda (curr total)
                (if (string=? elem curr)
                  (add1 total)
                  total))
              0
              lst))

          (define (helper-user user remaining visited result)
            (helper-lou
              (if (member (user-name user) visited)
                remaining
                (append (user-followings user) remaining))
              (cons (user-name user) visited)
              (compare user
                       result
                       (add1 (count-occurence (user-name user) visited)))))
          (define (helper-lou remaining visited result)
            (cond [(empty? remaining) (result-user result)]
                  [else
                   (helper-user (first remaining)
                                (rest remaining)
                                visited
                                result)]))]
    (helper-user user0 empty empty (make-result user0 -1))))


(check-expect (most-followers U1) (first (user-followings U1)))
(check-expect (most-followers U2) U2)
(check-expect (most-followers U3) (first (user-followings U3)))


;; PROBLEM 2:
;;
;; In UBC's version of How to Code, there are often more than 800 students taking
;; the course in any given semester, meaning there are often over 40 Teaching Assistants.
;;
;; Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;; a program to do it for us!
;;
;; Below are some data definitions for a simplified version of a TA schedule. There are some
;; number of slots that must be filled, each represented by a natural number. Each TA is
;; available for some of these slots, and has a maximum number of shifts they can work.
;;
;; Design a search program that consumes a list of TAs and a list of Slots, and produces one
;; valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;; maximum shifts. If no such schedules exist, produce false.
;;
;; You should supplement the given check-expects and remember to follow the recipe!


;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


(define (schedule-tas tas slots) empty) ;stub
