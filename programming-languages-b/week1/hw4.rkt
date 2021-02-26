#lang racket

(provide (all-defined-out))


;; Problem 1:
;; Produce a list of numbers from low to high separated by stride in sorted order.
;; Assumption: stride is positive

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; Problem 2:
;; Returns a list of string with each element of the given string appended with
;; the given suffix

(define (string-append-map los suffix)
  (map (lambda (s) (string-append s suffix)) los))


;; Problem 3:
;; Return the ith element of the list where i is the remainder produced when
;; dividing n by the list's length
;; Raises an exception if the list is empty or n is negative

(define (list-nth-mod lox n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? lox) (error "list-nth-mod: empty list")]
        [else (car (list-tail lox (remainder n (length lox))))]))


;; Problem 4:
;; Returns a list of the first n values produced by the stream s in order.
;; Assumption: n is non-negative

(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (let ([pair (stream)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))


;; Problem 5:
;; Returns a stream which produces natural numbers except numbers divisible by
;; 5 are negated

(define funny-number-stream
  (letrec ([stream (lambda (n)
                     (cons (if (= (remainder n 5) 0) (- n) n)
                           (lambda () (stream (+ n 1)))))])
    (lambda () (stream 1))))


;; Problem 6:
;; Returns a stream whose elements alternate between the strings
;; "dan.jpg" and "dog.jpg"

(define dan-then-dog
  (letrec ([stream (lambda (s)
                     (cons s (lambda ()
                               (stream (if (string=? s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (stream "dan.jpg"))))


;; Problem 7:
;; Produces a stream where the following condition holds true:
;; If the given stream s would produce v for its ith element, then the returned
;; stream would produce the pair (0 . v)

(define (stream-add-zero s)
  (lambda ()
    (let ([pair (s)])
      (cons (cons 0 (car pair))
            (stream-add-zero (cdr pair))))))


;; Problem 8:
;; Returns a stream whose elements are pairs where the first part if from xs
;; and the second part is from yz. The stream cycles forever through the lists.

(define (cycle-lists xs ys)
  (letrec ([stream (lambda (n)
                     (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (stream (+ n 1)))))])
    (lambda () (stream 0))))


;; Problem 9:
;; Returns #f if no vector element is a pair with a car field equal to v, else
;; return the first pair with an equal car field

(define (vector-assoc v vec)
  ; Vector length is constant, so define it outside the loop to avoid computing
  ; it again for each iteration.
  (letrec ([vlen (vector-length vec)]
           [loop (lambda (pos)
                   (if (= pos vlen)
                       #f
                       (let ([elem (vector-ref vec pos)])
                         (if (and (pair? elem) (equal? v (car elem)))
                           elem
                           (loop (+ pos 1))))))])
    (loop 0)))


;; Problem 10:
;; Returns a function that takes v and returns the same thing that (assoc v xs)
;; would return. This implements n-element cache of recent results to possible
;; make the assoc function faster.

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [slot-pos 0])
    (lambda (v)
      (let ([try (vector-assoc v cache)])
        (if try
          try
          (let ([ans (assoc v xs)])
            (if ans
                (begin (vector-set! cache (remainder slot-pos n) ans)
                       (set! slot-pos (+ slot-pos 1))
                       ans)
                ans)))))))


;; Problem 10 verbose version:
#; ; (#;) <-- This is a way to comment out an expression block
(define (cached-assoc xs n)
  (let ([cache (begin (printf "Initializing cache with ~a slots...\n" n)
                      (make-vector n #f))]
        [slot-pos (begin (printf "Setting slot-pos to 0...\n") 0)])
    (lambda (v)
      (let ([try (begin (printf "Trying for v = ~a \n" v)
                        (vector-assoc v cache))])
        (if try
          (begin (printf "Found ~a in cache: ~a \n" v cache) try)
          (let ([ans (begin (printf "No cache hit! Getting ans for ~a in ~a \n" v xs)
                            (assoc v xs))])
            (if ans
                (begin (printf "Found ans for ~a = ~a \n" v ans)
                       (vector-set! cache (remainder slot-pos n) ans)
                       (printf "Cache added for ~a => ~a \n" v cache)
                       (printf "Updating slot-pos from ~a " slot-pos)
                       (set! slot-pos (+ slot-pos 1))
                       (printf "to ~a \n" slot-pos)
                       ans)
                (begin (printf "No ans found for ~a \n" v) ans))))))))


;; ------------------ Challenge problem ------------------
;; Assumption: e1 and e2 produce numbers

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([v1 e1])
       (letrec ([loop (lambda ()
                        (if (< e2 v1)
                        (loop)
                        #t))])
         (loop)))]))


;; ------------------ CSE 341 problems --------------------
;; Solution for only the problems which are different from the Coursera assignment.


;; Efficient implementation of cycle-lists using function closure
;; As the given lists are always available in the lexical environment, we can
;; use it once the lists become empty.
(define (cycle-lists-challenge lox loy)
  (letrec ([stream (lambda (xs ys)
                     (let ([xs (if (null? xs) lox xs)]
                           [ys (if (null? ys) loy ys)])
                       (cons (cons (car xs) (car ys))
                             (lambda () (stream (cdr xs) (cdr ys))))))])
    (lambda () (stream lox loy))))


;; Testing both implementations:
(define test-cycle-lists
  (lambda ()
    (begin
      (define list1-length 1000)
      (define list2-length 1500)
      (define iterations 1000000)
      (printf (string-append
                "First list length: ~a\n"
                "Second list length: ~a\n"
                "Iterations: ~a\n")
              list1-length list2-length iterations)
      (printf "\nResults for cycle-lists-challenge function:\n")
      (time (stream-for-n-steps
              (cycle-lists-challenge
                (build-list list1-length identity)
                (build-list list2-length identity))
              iterations)
            (void))
      (printf "\nResults for cycle-lists function:\n")
      (time (stream-for-n-steps
              (cycle-lists
                (build-list list1-length identity)
                (build-list list2-length identity))
              iterations)
            (void)))))


;; Using LRU (Least Recently Used) policy to decide which cache slot to replace.
;;
;; This is a bit complicated but let me explain:
;; This is using a circular queue where the left side of the cache are where the
;; least recently accessed slots are and on the right side the most recently used
;; slots are present.
;; Now, we add to the cache normally until the cache is full and then we start
;; implementing the LRU technique.
;; If the cache is found, then we move that entry to the end of the cache,
;; otherwise we add the answer at the right end of the cache removing the first
;; entry which is the least recently used.
(define (cached-assoc-lru xs n)
  (let* ([cache (make-vector n #f)]
         [next-slot 0]
         [full? #f]
         ;; Same implementation of vector-assoc except that this will return
         ;; the slot position as well, if found, which is used to move the elements
         ;; in a circular queue manner.
         [vector-assoc
           (lambda (v vec)
             (letrec ([loop (lambda (pos)
                              (if (= pos n)
                                  #f
                                  (let ([elem (vector-ref vec pos)])
                                    (if (and (pair? elem) (equal? v (car elem)))
                                      (cons pos elem)
                                      (loop (+ pos 1))))))])
               (loop 0)))]
         ;; Move the given element at the end, keeping the order of other elements
         ;; as it is. The given elem argument is a pair containing the slot position
         ;; of the given element and the element itself.
         [move-to-end
           (lambda (elem)
             (let ([pos (car elem)])
               (begin
                 (set! cache (vector-append
                               (vector-take cache pos)
                               (vector-take-right cache (- n pos 1))
                               (vector (cdr elem)))))))]
         ;; Add the given element at the right end, removing the first element
         ;; from the cache. The given elem argument is the element which needs
         ;; to be added at the end of the cache.
         [add-at-end
           (lambda (elem)
             (begin
               (set! cache (vector-append
                             (vector-drop cache 1)
                             (vector elem)))))])
    ; The logic is as follows:
    ; Check whether the requested element is present in the cache. If it is and
    ; the cache is full, then move the element at the right end making it the
    ; most recently used cache. If the cache is not full, just return the element.
    ; Now, if the element is not present in the cache, get the answer and if the
    ; ans is false, return the ans. Otherwise, if the cache is full, add the
    ; ans at the right end and if the cache is not full, add it to the next-slot
    ; position, update the next-slot position and check whether the slot is full
    ; or not and update the full? variable accordingly.
    (lambda (val)
      (let ([cache-elem (vector-assoc val cache)])
        (if cache-elem
            (if full?
                (begin (move-to-end cache-elem) (cdr cache-elem))
                (cdr cache-elem))
            (let ([ans (assoc val xs)])
              (if ans
                  (if full?
                      (begin (add-at-end ans) ans)
                      (begin (vector-set! cache next-slot ans)
                             (set! next-slot (+ 1 next-slot))
                             (if (= next-slot n)
                                 (begin (set! full? #t) ans)
                                 ans)))
                  ans)))))))
