#lang htdp/asl

;; PROBLEM:
;;
;; Using the following data definition, design a function that consumes a room and produces
;; the total number of rooms reachable from the given room. Include the starting room itself.
;; Your function should be tail recursive, but you should not use the primitive length function.


;; Data Definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

(define H3
  (shared
    ([-0- (make-room "A" (list (make-room "B" (list (make-room "C" (list -0-))))))])
    -0-))

(define H4
  (shared
    ([-0- (make-room "A" (list -1- (make-room "D" (list -2-))))]
     [-1- (make-room "B" (list (make-room "C" (list -1-)) -2-))]
     [-2- (make-room "E" (list (make-room "F" empty) -0-))])
    -0-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist,
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (helper-room r rem visited)
            (if (member (room-name r) visited)
                (helper-lor rem visited)
                (helper-lor (append (room-exits r) rem)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (helper-lor rem visited)
            (cond [(empty? rem) (...)]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited)]))]
    (helper-room r0 empty empty)))


;; Room -> Natural
;; Produce the total number of rooms reachable from the given room, including the given room

(define (count-rooms r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; total is Natural; represents the count of rooms visited so far
  (local [(define (helper-room r rem visited total)
            (if (member (room-name r) visited)
                (helper-lor rem visited total)
                (helper-lor (append (room-exits r) rem)
                            (cons (room-name r) visited)
                            (add1 total)))) ; (... (room-name r))
          (define (helper-lor rem visited total)
            (cond [(empty? rem) total]
                  [else
                   (helper-room
                     (first rem)
                     (rest rem)
                     visited
                     total)]))]
    (helper-room r0 empty empty 0)))


(check-expect (count-rooms H1) 2)
(check-expect (count-rooms (first (room-exits H1))) 1)
(check-expect (count-rooms H2) 2)
(check-expect (count-rooms (first (room-exits H2))) 2)
(check-expect (count-rooms H4) 6)
