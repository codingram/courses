#lang htdp/asl

;; PROBLEM:
;;
;; Using the following data definition, design a function that produces the room with the most exits
;; (in the case of a tie you can produce any of the rooms in the tie).


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


;; Room -> Room
;; Produces the room with the most exits.
;; In the case of a tie, produce the first room with the most exits.

(define (most-exits r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; result is Room; result so far accumulator, room with the most exits so far
  (local [(define (helper-room r rem visited result)
            (if (member (room-name r) visited)
                (helper-lor rem visited result)
                (helper-lor (append (room-exits r) rem)
                            (cons (room-name r) visited)
                            (if (> (length (room-exits r)) (length (room-exits result)))
                              r
                              result))))
          (define (helper-lor rem visited result)
            (cond [(empty? rem) result]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited
                                result)]))]
    (helper-room r0 empty empty r0)))


(check-expect (most-exits H1) H1)
(check-expect (most-exits H2) H2)
(check-expect (most-exits H4) H4)
