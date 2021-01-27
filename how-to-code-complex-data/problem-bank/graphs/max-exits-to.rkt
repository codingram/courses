#lang htdp/asl

;; PROBLEM:
;;
;; Using the following data definition, design a function that produces the room to which the greatest
;; number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie).


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
#;
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
;; Produces the room to which the greatest number of other rooms have exits
;; Caveat: This algorithm will produce the first room with the max exits to if the
;;         the given room has no exits coming to. If the given room has exits coming to
;;         and the given room has the greatest number of exits to but is tied with some
;;         other room, the algorithm will produce the second room.

(define (max-exits-to r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; result is Result; represents the result so far
  (local [
          ; Result is (make-result-room Room Natural)
          ; interp. result data comprising of the current largest room and the number
          ;         of times it was encountered
          (define-struct result (room encountered))

          (define (helper-room r rem visited result)
            (let ([compare
                    (if (string=? (room-name r) (room-name (result-room result)))
                      (make-result (result-room result) (add1 (result-encountered result)))
                      (if (> (result-encountered result) 0)
                        result
                        (make-result r 1)))])
              (if (member (room-name r) visited)
                  (helper-lor rem visited compare)
                  (helper-lor (append (room-exits r) rem)
                              (cons (room-name r) visited)
                              compare))))
          (define (helper-lor rem visited result)
            (cond [(empty? rem) (result-room result)]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited
                                result)]))]
    (helper-room r0 empty empty (make-result r0 -1))))


(check-expect (max-exits-to H1) (first (room-exits H1)))
(check-expect (max-exits-to H2) (first (room-exits H2)))
(check-expect (max-exits-to H4) (first (room-exits H4)))
