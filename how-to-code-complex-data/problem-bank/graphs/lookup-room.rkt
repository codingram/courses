#lang htdp/asl


;; PROBLEM:
;;
;; Using the following data definition, design a function that consumes a room and a room
;; name and tries to find a room with the given name starting at the given room.


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


;; Room String -> Boolean
;; Produces true if the room name is found in the given room, false otherwise

(define (lookup-room? r0 rn)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (helper-room r rem visited)
            (if (string=? (room-name r) rn)
              true
              (if (member (room-name r) visited)
                  (helper-lor rem visited)
                  (helper-lor (append (room-exits r) rem)
                              (cons (room-name r) visited)))))
          (define (helper-lor rem visited)
            (cond [(empty? rem) false]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited)]))]
    (helper-room r0 empty empty)))


(check-expect (lookup-room? H1 "A") true)
(check-expect (lookup-room? H1 "C") false)
(check-expect (lookup-room? H3 "C") true)
(check-expect (lookup-room? H4 "F") true)
(check-expect (lookup-room? H4 "G") false)
