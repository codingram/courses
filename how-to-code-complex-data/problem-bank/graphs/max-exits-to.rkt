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

(define H5
  (shared
    ([-0- (make-room "A" (list -1-
                               (make-room "D" (list -2-))
                               (make-room "G" (list -2-))))]
     [-1- (make-room "B" (list (make-room "C" (list -1-)) -2-))]
     [-2- (make-room "E" (list (make-room "F" empty) -0-))])
    -0-))

(define H6
  (shared
    ([-0- (make-room "A" (list -1- (make-room "D" (list -0-))))]
     [-1- (make-room "B" (list (make-room "C" (list -1-))
                               (make-room "E" (list -0-))))])
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
;; Invariant: In case of tie, produce the first room

(define (max-exits-to r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; result is Result; represents the result so far
  (local [
          ;; Result is (make-result-room Room Natural)
          ;; interp. result data comprising of the current largest room and the number
          ;;         of times it was total
          (define-struct result (room total))

          ;; Room Room Natural -> Room
          ;; Compare the current room with the current result room
          ;; If the room name is equal to the result room name, add 1 to the result total,
          ;; otherwise check whether the current room occurs more times in visited than the
          ;; current result total and update the result accordingly.
          (define (compare room result nvisits)
            (if (string=? (room-name room) (room-name (result-room result)))
              (make-result (result-room result) (add1 (result-total result)))
              (if (> nvisits (result-total result))
                (make-result room nvisits)
                result)))

          ;; String (listof String) -> Natural
          ;; Produce the number of times elem occurs in lst
          (define (count-occurence elem lst)
            (foldl
              (lambda (curr total)
                (if (string=? elem curr)
                  (add1 total)
                  total))
              0
              lst))

          (define (helper-room r rem visited result)
            (helper-lor
              (if (member (room-name r) visited)
                rem
                (append (room-exits r) rem))
              (cons (room-name r) visited)
              (compare r
                       result
                       ;; Add the current occurence as well.
                       ;; For every node visited the first time, the nvisits value is 1 which seems to be an
                       ;; important tie breaker when the number of exits leading to the first node is equal to
                       ;; some other node. Now, as the first node comes first, the actual result for
                       ;; max-exits-to should be the first node in case of a tie. This maintains that invariant.
                       (add1 (count-occurence (room-name r) visited)))))
          (define (helper-lor rem visited result)
            (cond [(empty? rem) (result-room result)]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited
                                result)]))]
    (helper-room r0 empty empty (make-result r0 -1))))


(check-expect (max-exits-to H1) (first (room-exits H1)))
; For H2, H3 and H6, it should show the first max exits to in case of tie
(check-expect (max-exits-to H2) H2)
(check-expect (max-exits-to H3) H3)
(check-expect (max-exits-to H6) H6)
(check-expect (max-exits-to H4) (first (room-exits H4)))
(check-expect (max-exits-to H5) (second (room-exits (first (room-exits H5)))))
