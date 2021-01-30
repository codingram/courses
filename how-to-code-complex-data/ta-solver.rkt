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

(define ERIKA (make-ta "Erika" 1 (list 1 3 7 9)))
(define RYAN (make-ta "Ryan" 1 (list 1 8 10)))
(define REECE (make-ta "Reece" 1 (list 5 6)))
(define KATIE (make-ta "Katie" 1 (list 4 6)))
(define ALEX (make-ta "Alex" 1 (list 7)))
(define ERIN (make-ta "Erin" 1 (list 4)))
(define GORDON (make-ta "Gordon" 2 (list 2 3 9)))
(define DAVID (make-ta "David" 2 (list 2 8 9)))
(define AASHISH (make-ta "Aashish" 2 (list 1 10)))
(define GRANT (make-ta "Grant" 2 (list 1 11)))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12)))

(define QUIZ-TAs-1 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE))
(define QUIZ-TAs-2 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ALEX))
(define QUIZ-TAs-3 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ERIN))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; -------------
;; Functions


;; (listof TA) (listof Slot) -> Schedule or false
;; Produce valid schedule given TAs and Slots; false if impossible

;; Cross Product of Types Table:
;;
;;  ╔══════════════════════╦═══════════════╦════════════════════════════════╗
;;  ║                      ║               ║                                ║
;;  ║        (listof Slot) ║     empty     ║  (cons Slot (listof Slot))     ║
;;  ║                      ║               ║                                ║
;;  ║ (listof TA)          ║               ║                                ║
;;  ╠══════════════════════╬═══════════════╬════════════════════════════════╣
;;  ║                      ║               ║                                ║
;;  ║ empty                ║               ║        false                   ║
;;  ║                      ║               ║                                ║
;;  ╠══════════════════════╣     empty     ╠════════════════════════════════╣
;;  ║                      ║               ║                                ║
;;  ║ (cons TA (listof TA) ║               ║ <Backtracking search template> ║
;;  ║                      ║               ║                                ║
;;  ╚══════════════════════╩═══════════════╩════════════════════════════════╝

;; Template used:
;; - 2 one-of using the types table
;; - Arbitrary arity tree with local
;; - Generative recursion
;; - Backtracking search

(define (schedule-tas tas slots)
  (cond [(empty? slots) empty]
        [(empty? tas) false]
        [else
          (local
            [
             ;; (listof TA) (listof Slot) Schedule -> Schedule or false
             (define (helper-schedule tas slots schedule)
               (if (filled? schedule slots)
                 schedule
                 (helper-schedule-list tas slots (next-schedules tas slots schedule))))

             ;; (listof TA) (listof Slot) (listof Schedule) -> Schedule or false
             (define (helper-schedule-list tas slots schedule-list)
               (cond [(empty? schedule-list) false]
                     [else
                       (let ([try (helper-schedule tas slots (first schedule-list))])
                         (if (not (false? try))
                           try
                           (helper-schedule-list tas slots (rest schedule-list))))]))]
            (helper-schedule tas slots empty))]))


;; Schedule (listof Slot) -> Boolean
;; Determine whether the current schedule is filled as per the required slots
;; ASSUMPTION: All assignments are valid

(define (filled? schedule slots)
  ; This check is not necessary but it will create a short circuit behavior when
  ; the Schedule is empty at the expense of one expression.
  (cond [(empty? schedule) false]
        [else
          (andmap (lambda (slot)
                    (slot-present? slot schedule))
                  slots)]))


;; Slot Schedule -> Boolean
;; Determine whether the given slot is present in the given Schedule
;; ASSUMPTION: All assignments are valid

(define (slot-present? slot schedule)
  (ormap
    (lambda (assignment)
      (= (assignment-slot assignment) slot))
    schedule))


;; (listof TA) (listof Slot) Schedule -> (listof Schedule)
;; Produces the next set of possible schedule down the search tree

(define (next-schedules tas slots schedule)
  (map (lambda (assignment)
         (cons assignment schedule))
       (filter (lambda (assignment)
                 (valid-assignment? assignment schedule))
               (fill-slot-with-tas
                 (next-empty-slot slots schedule)
                 tas
                 schedule))))


;; (listof Slot) Schedule -> Slot or false
;; Produces the next empty slot available, false if there are no empty slots

(define (next-empty-slot slots schedule)
  (cond [(empty? slots) false]
        [else
          (if (slot-present? (first slots) schedule)
            (next-empty-slot (rest slots) schedule)
            (first slots))]))


;; (Slot or false) (listof TA) Schedule -> Schedule
;; Produces the next schedule by assigning all the TAs to the given slot

(define (fill-slot-with-tas slot tas schedule)
  (cond [(false? slot) empty]
        [else
          (map (lambda (ta)
                 (make-assignment ta slot))
               tas)]))


;; Assignment Schedule -> Boolean
;; Determine whether the given assignment is valid or not.

(define (valid-assignment? assignment schedule)
  (let ([assigned-ta (assignment-ta assignment)])
    (and (member (assignment-slot assignment) (ta-avail assigned-ta))
         (< (total-ta-assignments assigned-ta schedule) (ta-max assigned-ta)))))


;; TA Schedule -> Natural
;; Count the total assignments of the given TA in the given schedule

(define (total-ta-assignments ta schedule)
  (foldl (lambda (assignment total)
           (if (string=? (ta-name ta) (ta-name (assignment-ta assignment)))
             (add1 total)
             total))
         0
         schedule))


;; --------------
;; Tests

(check-expect (total-ta-assignments SOBA empty) 0)
(check-expect (total-ta-assignments SOBA (list (make-assignment SOBA 1))) 1)
(check-expect (total-ta-assignments SOBA (list (make-assignment UDON 1))) 0)

(check-expect (valid-assignment? (make-assignment SOBA 1) empty) true)
(check-expect (valid-assignment? (make-assignment SOBA 3) (list (make-assignment SOBA 1))) true)
(check-expect (valid-assignment? (make-assignment SOBA 2) empty) false)
(check-expect (valid-assignment? (make-assignment UDON 3) (list (make-assignment UDON 4))) false)

(check-expect (fill-slot-with-tas false (list SOBA) empty) empty)
(check-expect (fill-slot-with-tas 1 (list SOBA UDON) empty)
              (list (make-assignment SOBA 1) (make-assignment UDON 1)))
(check-expect (fill-slot-with-tas 2 (list SOBA UDON) (list (make-assignment SOBA 1)))
              (list (make-assignment SOBA 2) (make-assignment UDON 2)))


(check-expect (next-empty-slot '(1 2) empty) 1)
(check-expect (next-empty-slot '(1 2) (list (make-assignment SOBA 1))) 2)
(check-expect (next-empty-slot '(1 3) (list (make-assignment SOBA 1)
                                            (make-assignment SOBA 3)))
              false)

;; Empty TAs and Slots won't be passed onto 'next-schedules' function as they are
;; filtered using the 2 one-of design
(check-expect (next-schedules (list SOBA) '(1) empty) (list (list (make-assignment SOBA 1))))
(check-expect (next-schedules (list SOBA) '(1 2 3) empty) (list (list (make-assignment SOBA 1))))
(check-expect (next-schedules (list SOBA UDON) '(1 2 3) empty) (list (list (make-assignment SOBA 1))))
(check-expect (next-schedules (list SOBA UDON) '(1 2 3) (list (make-assignment SOBA 1)))
              empty)
(check-expect (next-schedules NOODLE-TAs '(1 2 3) (list (make-assignment SOBA 1)))
              (list (list (make-assignment RAMEN 2) (make-assignment SOBA 1))))

;; Empty Schedule is handled directly in the 'filled?' function
(check-expect (slot-present? 1 (list (make-assignment SOBA 1))) true)
(check-expect (slot-present? 1 (list (make-assignment SOBA 3))) false)
(check-expect (slot-present? 3 (list (make-assignment SOBA 1) (make-assignment SOBA 3))) true)

;; Empty list of Slot case is handled directly in the 'schedule-tas' function
(check-expect (filled? empty '(1 2)) false)
(check-expect (filled? (list (make-assignment SOBA 1)) '(1 2)) false)
(check-expect (filled? (list (make-assignment SOBA 3)) '(1 3)) false)
(check-expect (filled? (list (make-assignment SOBA 1)) '(1)) true)
(check-expect (filled? (list (make-assignment SOBA 1) (make-assignment SOBA 3)) '(1 3)) true)

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
