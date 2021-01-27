#lang htdp/asl


;; PROBLEM:
;;
;; Using the following data definition:
;;
;; a) Design a function that consumes a room and produces a list of the names of
;;    all the rooms reachable from that room.
;;
;; b) Revise your function from (a) so that it produces a list of the rooms
;;    not the room names


;; Data Definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to


(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define H4
  (shared
    ([-0- (make-room "A" (list -1- (make-room "D" (list -2-))))]
     [-1- (make-room "B" (list (make-room "C" (list -1-)) -2-))]
     [-2- (make-room "E" (list (make-room "F" empty) -0-))])
    -0-))

;; template: structural recursion, encapsulate w/ local,
;;           context-preserving accumulator what rooms traversed on this path
#;
(define (fn-for-house r0)
  ;; path is (listof String); context preserving accumulator, names of rooms
  (local [(define (fn-for-room r  path)
            (if (member (room-name r) path)
                (... path)
                (fn-for-lor (room-exits r)
                            (cons (room-name r) path))))
          (define (fn-for-lor lor path)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-room (first lor) path)
                        (fn-for-lor (rest lor) path))]))]
    (fn-for-room r0 empty)))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist,
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))


;; Room -> (listof String)
;; Produce list of room names reachable from the given room

(define (reachable-room-names r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (helper-room r rem visited)
            (if (member (room-name r) visited)
                (helper-lor rem visited)
                (helper-lor (append (room-exits r) rem)
                            ; We want to add the current room to the end of the visited list
                            ; thus making the list in chronological order
                            (append visited (list (room-name r))))))
          (define (helper-lor rem visited)
            (cond [(empty? rem) visited]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited)]))]
    (helper-room r0 empty empty)))


(check-expect (reachable-room-names H1) '("A" "B"))
(check-expect (reachable-room-names (first (room-exits H1))) '("B"))
(check-expect (reachable-room-names H2) '("A" "B"))
(check-expect (reachable-room-names (first (room-exits H2))) '("B" "A"))
(check-expect (reachable-room-names H4) '("A" "B" "C" "E" "F" "D"))
(check-expect (reachable-room-names (first (room-exits H4))) '("B" "C" "E" "F" "A" "D"))


;; Room -> (listof String)
;; Produce list of room names reachable from the given room

(define (reachable-rooms r0)
  ;; rem is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (helper-room r rem visited)
            (if (member r visited)
                (helper-lor rem visited)
                (helper-lor (append (room-exits r) rem)
                            ; We want to add the current room to the end of the visited list
                            ; thus making the list in chronological order
                            (append visited (list r)))))
          (define (helper-lor rem visited)
            (cond [(empty? rem) visited]
                  [else
                   (helper-room (first rem)
                                (rest rem)
                                visited)]))]
    (helper-room r0 empty empty)))


(check-expect (reachable-rooms H1)
              (list H1 (first (room-exits H1))))
(check-expect (reachable-rooms (first (room-exits H1)))
              (list (first (room-exits H1))))
(check-expect (reachable-rooms H2)
              (list H2 (first (room-exits H2))))
(check-expect (reachable-rooms H4)
              (list H4
                    (first (room-exits H4))
                    (first (room-exits (first (room-exits H4))))
                    (second (room-exits (first (room-exits H4))))
                    (first (room-exits (second (room-exits (first (room-exits H4))))))
                    (second (room-exits H4))))
