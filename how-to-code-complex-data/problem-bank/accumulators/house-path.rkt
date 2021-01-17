#lang htdp/asl


;; Consider the following house diagram:
;;
;;   +---  ---+------------+
;;   | Porch  |  Living    |
;;   |            Room     |
;;   |--------|            |
;;   | Dining              |
;;   | Room   |-  -+-------|
;;   |        |    | Study |
;;   |        |            |
;;   |        |    |-------|
;;   |--- ----| H  | Bed-  |
;;   |        | a    Room  |
;;   | Kitchen| l  |       |
;;   |        | l  |-------|
;;   |        |    | Bath- |
;;   |               Room  |
;;   |        |    |       |
;;   +--------+----+-------+
;;
;;
;;
;; Starting from the porch, there are many paths through the house that you can
;; follow without retracing your steps.  If we represent these paths as lists:
;; (list
;;  (list "Porch")
;;  (list "Porch" "Living Room")
;;  (list "Porch" "Living Room" "Hall")
;;  ...)
;;
;; you can see that a lot of these paths start with the same sequence of rooms.
;; We can represent these paths, and capture their shared initial parts, by using
;; a tree:
;;
;;                           Porch
;;                             |
;;                        Living Room
;;                             |
;;               +-------------+-------------+
;;               |                           |
;;         Dining Room                      Hall
;;               |                           |
;;               |             +--------+----+----+---------+
;;               |             |        |         |         |
;;            Kitchen       Kitchen   Study   Bathroom   Bedroom
;;               |             |
;;              Hall     Dining Room
;;               |
;;     +---------+---------+
;;     |         |         |
;;   Study   Bedroom   Bathroom
;;
;; The following data definition does exactly this.

(define-struct path (room nexts))
;; Path is (make-path String (listof Path))
;; interp. An arbitrary-arity tree of paths.
;;  - (make-path room nexts) represents all the paths downward from room
(define PB (make-path "Bathroom" empty)) ; a room from which there are no paths

(define PP
  (make-path "Porch"
   (list
    (make-path "Living Room"
      (list (make-path "Dining Room"
              (list (make-path"Kitchen"
                      (list (make-path "Hall"
                              (list (make-path "Study" (list))
                                    (make-path "Bedroom" (list))
                                    (make-path "Bathroom" (list))))))))
            (make-path "Hall"
              (list (make-path "Kitchen"
                      (list (make-path "Dining Room" (list))))
                    (make-path "Study" (list))
                    (make-path "Bedroom" (list))
                    (make-path "Bathroom" (list)))))))))

#;
(define (fn-for-path p)
  (local [(define (fn-for-path p)
            (... (path-room p)
                 (fn-for-lop (path-nexts p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-path (first lop))
                        (fn-for-lop (rest lop)))]))]
    (fn-for-path p)))



;; The problems below also make use of the following data definition and function:

;; Result is one of:
;; - Boolean
;; - "never"
;; interp. three possible answers to a question
(define R0 true)
(define R1 false)
(define R2 "never")

#;
(define (fn-for-result r)
  (cond
    [(boolean? r) (... r)]
    [else (...)]))

;; Result Result -> Result
;; produce the logical combination of two results

;; Cross Product of Types Table:
;;
;;  ╔════════════════╦═══════════════╦══════════════╗
;;  ║                ║               ║              ║
;;  ║            r0  ║   Boolean     ║   "never"    ║
;;  ║                ║               ║              ║
;;  ║    r1          ║               ║              ║
;;  ╠════════════════╬═══════════════╬══════════════╣
;;  ║                ║               ║              ║
;;  ║   Boolean      ║ (and r0 r1)   ║              ║
;;  ║                ║               ║              ║
;;  ╠════════════════╬═══════════════╣  r1          ║
;;  ║                ║               ║              ║
;;  ║   "never"      ║  r0           ║              ║
;;  ║                ║               ║              ║
;;  ╚════════════════╩═══════════════╩══════════════╝


(check-expect (and-result false false) false)
(check-expect (and-result false true) false)
(check-expect (and-result false "never") false)
(check-expect (and-result true false) false)
(check-expect (and-result true true) true)
(check-expect (and-result true "never") true)
(check-expect (and-result "never" true) true)
(check-expect (and-result "never" false) false)
(check-expect (and-result "never" "never") "never")

(define (and-result r0 r1)
  (cond [(and (boolean? r0) (boolean? r1)) (and r0 r1)]
        [(string? r0) r1]
        [else r0]))



;; PROBLEM 1:
;;
;; Design a function called always-before that takes a path tree p and two room
;; names b and c, and determines whether starting from p:
;; 1) you must pass through room b to get to room c (produce true),
;; 2) you can get to room c without passing through room b (produce false), or
;; 3) you just can't get to room c (produce "never").
;;
;; Note that if b and c are the same room, you should produce false since you don't
;; need to pass through the room to get there.


;; Path String String -> Result
;; Determine whether we can get to 'dest' or not and also if we need to pass through 'via' or not.

(define (always-before path via dest)
  ; passed? :Boolean; represents whether we passed through the 'via' path
  (local [(define (helper-path path passed?)
            (if (string=? (path-room path) dest)
              passed?
              (helper-lop (path-nexts path)
                          (if (false? passed?)
                            (string=? (path-room path) via)
                            true))))
          (define (helper-lop lop passed?)
            (cond [(empty? lop) "never"]
                  [else
                   (and-result (helper-path (first lop) passed?)
                               (helper-lop (rest lop) passed?))]))]
    (helper-path path false)))


(check-expect (always-before PB "Hall" "Study") "never")
(check-expect (always-before PB "Bathroom" "Bathroom") false)
(check-expect (always-before PP "Kitchen" "Bathroom") false)
(check-expect (always-before PP "Hall" "Dining Room") false)
(check-expect (always-before PP "Kitchen" "Living Room") false)
(check-expect (always-before PP "Study" "Bathroom") false)
(check-expect (always-before PP "Hall" "Study") true)
(check-expect (always-before PP "Living Room" "Bathroom") true)


;; OPTIONAL EXTRA PRACTICE PROBLEM:
;;
;; Once you have always-before working, make a copy of it, rename the copy to
;; always-before-tr, and then modify the function to be tail recursive.


;; Path String String -> Result
;; Determine whether we can get to 'dest' or not and also if we need to pass through 'via' or not
;; using tail recursion.

(define (always-before-tr path via dest)
  ; passed? is Boolean; represents whether we passed through the 'via' path
  ; rem-path is (listof Entry); represents a worklist accumulator
  ; result is Result; represents the result for everything we have seen so far
  (local [
          ; Entry is (make-entry Path Boolean)
          ; interp. a worklist entry
          (define-struct entry (path passed?))

          (define (helper-path rem-path path passed? result)
            (if (string=? (path-room path) dest)
              (helper-lop rem-path (and-result passed? result))
              (helper-lop
                (append (map
                          (lambda (next-path)
                            (if (false? passed?)
                              (make-entry next-path
                                          (string=? (path-room path) via))
                              (make-entry next-path true)))
                          (path-nexts path))
                        rem-path)
                result)))

          (define (helper-lop rem-path result)
            (cond [(empty? rem-path) result]
                  [else
                    (helper-path
                      (rest rem-path)
                      (entry-path (first rem-path))
                      (entry-passed? (first rem-path))
                      result)]))]
    (helper-path empty path false "never")))


(check-expect (always-before-tr PB "Hall" "Study") "never")
(check-expect (always-before-tr PB "Bathroom" "Bathroom") false)
(check-expect (always-before-tr PP "Kitchen" "Bathroom") false)
(check-expect (always-before-tr PP "Hall" "Dining Room") false)
(check-expect (always-before-tr PP "Kitchen" "Living Room") false)
(check-expect (always-before-tr PP "Study" "Bathroom") false)
(check-expect (always-before-tr PP "Hall" "Study") true)
(check-expect (always-before-tr PP "Living Room" "Bathroom") true)
