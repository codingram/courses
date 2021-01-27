#lang htdp/asl


;; PROBLEM:
;;
;; Imagine you are suddenly transported into a mysterious house, in which all
;; you can see is the name of the room you are in, and any doors that lead OUT
;; of the room.  One of the things that makes the house so mysterious is that
;; the doors only go in one direction. You can't see the doors that lead into
;; the room.
;;
;; Here are some examples of such a house:
;;
;; A --> B
;;
;; +-----+
;; |     |
;; v     |
;; A --> B
;;
;;
;; A ------> B
;; ^        /
;;  \      /
;;   \    /
;;    \  v
;;     C
;;
;;            +---------+
;;            |         |
;;            v         |
;;  A ------> B ------> C
;;  ^ \        \
;;  |  \        \
;;  |   \        v
;;  |    > D ---> E ---> F
;;  |             |
;;  +-------------+
;;
;; In computer science, we refer to such an information structure as a directed
;; graph. Like trees, in directed graphs the arrows have direction. But in a
;; graph it is  possible to go in circles, as in the second example above. It
;; is also possible for two arrows to lead into a single node, as in the fourth
;; example.
;;
;;
;; Design a data definition to represent such houses. Also provide example data
;; for the four houses above.

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. name of the room and list of rooms that the exists lead to
(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2 (shared ([-0- (make-room "A" (list (make-room "B" (list -0-))))]) -0-))

#;
(define H3
  (shared
    ([-0- (make-room "A" (list (make-room "B" (list (make-room "C" (list -0-))))))])
    -0-))

;; Alternate version of H3, defining each node as shared
(define H3
  (shared
    ([-A- (make-room "A" (list -B-))]
     [-B- (make-room "B" (list -C-))]
     [-C- (make-room "C" (list -A-))])
    -A-))

;; (define H4
;;   (shared
;;     ([-0-
;;        (make-room
;;          "A" (list
;;                (shared
;;                  ([-1- (make-room "B" (list
;;                                         (make-room "C" (list -1-))
;;                                         (make-room "E" (list -0- (make-room "F" empty)))))])
;;                  -1-)
;;                    (make-room "D" (list (make-room "E" (list -0- (make-room "F" empty)))))))])
;;     -0-))

;; As seen in the initial try, the room "E" is repeated and room "B" is constructed inside.
;; So, remove the repeatition and the inside creation.
(define H4
  (shared
    ([-0- (make-room "A" (list -1- (make-room "D" (list -2-))))]
     [-1- (make-room "B" (list -2- (make-room "C" (list -1-))))]
     [-2- (make-room "E" (list -0- (make-room "F" empty)))])
    -0-))

;; Template:
;; - Structural recursion
;; - Arbitrary arity tree with local
;; - Tail recursion with a worklist accumulator
;; - Context preserving accumulator stating which vertices we have passed through
#;
(define (fn-for-house h0)
  ; rem is (listof Room) ;represents a worklist accumulator
  ; visited is (listof String) ;represents list of house names we have visited so far
  (local
    [(define (helper-house h rem visited)
       (if (member (room-name h) visited)
         (helper-loh rem visited)
         (helper-loh (append (room-exits h) rem)
                     (cons (room-name h) visited))))
     (define (helper-loh loh visited)
       (cond [(empty? loh) (...)]
             [else
               (helper-house
                 (first loh)
                 (rest loh)
                 visited)]))]
    (helper-house h0 empty empty)))


;; PROBLEM:
;;
;; Design a function that consumes a Room and a room name, and produces true
;; if it is possible to reach a room with the given name starting at the given
;; room. For example:
;;
;;   (reachable? H1 "A") produces true
;;   (reachable? H1 "B") produces true
;;   (reachable? H1 "C") produces false
;;   (reachable? H4 "F") produces true
;;
;; But note that if you defined H4F to be the room named F in the H4 house then
;; (reachable? H4F "A") would produce false because it is not possible to get
;; to A from F in that house.

;; Room String -> Boolean
;; Produce true if starting at 'start' it is possible to reach a room named 'dest',
;; false otherwise


(define (reachable? start dest)
  ; rem is (listof Room) ;represents a worklist accumulator
  ; visited is (listof String) ;represents list of house names we have visited so far
  (local
    [(define (helper-house h rem visited)
       (if (string=? (room-name h) dest)
         true
         (if (member (room-name h) visited)
           (helper-loh rem visited)
           (helper-loh (append (room-exits h) rem)
                       (cons (room-name h) visited)))))
     (define (helper-loh loh visited)
       (cond [(empty? loh) false]
             [else
               (helper-house
                 (first loh)
                 (rest loh)
                 visited)]))]
    (helper-house start empty empty)))


(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? (first (room-exits H1)) "A") false)
(check-expect (reachable? H4 "F") true)
