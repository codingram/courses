#lang racket

(provide (all-defined-out))


;; Number Number Number -> (listof Number)
;; Produce a list of numbers from low to high separated by stride in sorted order.
;; Assumption: stride is positive

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))


;; (listof String) String -> (listof String)
;; Returns a list of string with each element of the given string appended with
;; the given suffix

(define (string-append-map los suffix)
  (map (lambda (s) (string-append s suffix)) los))


;; (listof X) Number -> X
;; Return the ith element of the list where i is the remainder produced when
;; dividing n by the list's length
;; Raises an exception if the list is empty or n is negative

(define (list-nth-mod lox n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? lox) (error "list-nth-mod: empty list")]
        [else (car (list-tail lox (remainder n (length lox))))]))


;; Returns a list of the first n values produced by the stream s in order.
;; Assumption: n is non-negative

(define (stream-for-n-steps stream n)
  (if (= n 0)
      '()
      (let ([pair (stream)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))


;; Returns a stream which produces natural numbers except numbers divisible by
;; 5 are negated

(define funny-number-stream
  (letrec ([stream (lambda (n)
                     (cons (if (= (remainder n 5) 0) (- n) n)
                           (lambda () (stream (+ n 1)))))])
    (lambda () (stream 1))))


;; Returns a stream whose elements alternate between the strings
;; "dan.jpg" and "dog.jpg"

(define dan-then-dog
  (letrec ([stream (lambda (s)
                     (cons s (lambda ()
                               (stream (if (string=? s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (stream "dan.jpg"))))


;; Produces a stream where the following condition holds true:
;; If the given stream s would produce v for its ith element, then the returned
;; stream would produce the pair (0 . v)

(define (stream-add-zero s)
  (lambda ()
    (let ([pair (s)])
      (cons (cons 0 (car pair))
            (stream-add-zero (cdr pair))))))


;; Returns a stream whose elements are pairs where the first part if from xs
;; and the second part is from yz. The stream cycles forever through the lists.

(define (cycle-lists xs ys)
  (letrec ([stream (lambda (n)
                     (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (stream (+ n 1)))))])
    (lambda () (stream 0))))


;; Returns $f if no vector element is a pair with a car field equal to v, else
;; return the first pair with an equal car field

(define (vector-assoc v vec)
  (letrec ([vlen (vector-length vec)]
           [loop (lambda (pos)
                   (if (or (= vlen 0) (= pos vlen))
                     #f
                     (let ([elem (vector-ref vec pos)])
                       (if (and (pair? elem) (equal? v (car elem)))
                         elem
                         (loop (+ pos 1))))))])
    (loop 0)))


;; Returns a function that takes v and returns the same thing that (assoc v xs)
;; would return. This implements n-element cache of recent results to possible
;; make the assoc function faster.

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [slot-pos 0]
           [func (lambda (v)
                   (let ([try (vector-assoc v cache)])
                     (if try
                         try
                         (let ([ans (assoc v xs)])
                           (if ans
                               (begin (vector-set! cache (remainder slot-pos n) ans)
                                      (set! slot-pos (+ slot-pos 1))
                                      ans)
                               ans)))))])
    func))


;; Verbose version
#;
(define (cached-assoc xs n)
  (letrec ([cache (begin (printf "Initializing cache with ~a slots...\n" n) (make-vector n #f))]
           [slot-pos (begin (printf "Setting slot-pos to 0...\n") 0)]
           [func (lambda (v)
                   (let ([try (begin (printf "Trying for v = ~a \n" v) (vector-assoc v cache))])
                     (if try
                         (begin (printf "Found ~a in cache: ~a \n" v cache) try)
                         (let ([ans (begin (printf "No cache hit! Getting ans for ~a in ~a \n" v xs) (assoc v xs))])
                           (if ans
                               (begin (printf "Found ans for ~a = ~a \n" v ans)
                                      (vector-set! cache (remainder slot-pos n) ans)
                                      (printf "Cache added for ~a => ~a \n" v cache)
                                      (printf "Updating slot-pos from ~a " slot-pos)
                                      (set! slot-pos (+ slot-pos 1))
                                      (printf "to ~a \n" slot-pos)
                                      ans)
                               (begin (printf "No ans found for ~a \n" v) ans))))))])
    func))


;; Challenge problem

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [loop (lambda ()
                      (if (< e2 v1)
                        (loop)
                        #t))])
       (loop))]))
