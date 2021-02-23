#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code
;; will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require rackunit "hw4.rkt")

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   ;; sequence test
   (check-equal? (sequence 3 2 1) '())
   (check-equal? (sequence 0 5 1) '(0 1 2 3 4 5))
   (check-equal? (sequence 3 11 2) '(3 5 7 9 11))
   (check-equal? (sequence 3 8 3) '(3 6))
   (check-equal? (sequence 4 4 2) '(4))

   ;; string-append-map test
   (check-equal? (string-append-map '() ".jpg") '())
   (check-equal? (string-append-map '("some" "random") "") '("some" "random"))
   (check-equal? (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")
                 '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg"))

   ;; list-nth-mod test
   (check-exn exn:fail? (lambda () (list-nth-mod '() 1)))
   (check-exn exn:fail? (lambda () (list-nth-mod '(1 2 3) -1)))
   (check-equal? (list-nth-mod '(0 1 2 3 4 5 6) 10) 3)
   (check-equal? (list-nth-mod '(0 1 2 3 4) 2) 2)

   ;; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 0) '())
   (check-equal? (stream-for-n-steps ones 1) '(1))
   (check-equal? (stream-for-n-steps ones 2) '(1 1))

   ;; funny-number-stream test
   (check-true (procedure? (cdr (funny-number-stream))))
   (check-equal? (car (funny-number-stream)) 1)
   (check-equal? (stream-for-n-steps funny-number-stream 16)
                 (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16))

   ;; dan-then-dog test
   (check-true (procedure? (cdr (dan-then-dog))))
   (check-equal? (car (dan-then-dog)) "dan.jpg")
   (check-equal? (stream-for-n-steps dan-then-dog 3)
                 '("dan.jpg" "dog.jpg" "dan.jpg"))
   (check-equal? (stream-for-n-steps dan-then-dog 4)
                 '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg"))

   ;; stream-add-zero test
   (check-true (procedure? (stream-add-zero ones)))
   (check-true (procedure? (cdr ((stream-add-zero ones)))))
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) `(,(cons 0 1)))
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 5)
                 `(,(cons 0 1) ,(cons 0 2) ,(cons 0 3) ,(cons 0 4) ,(cons 0 (- 5))))


   ;; cycle-lists test
   (check-true (procedure? (cycle-lists '(1) '("a"))))
   (check-true (procedure? (cdr ((cycle-lists '(1 2) '("a"))))))
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3)
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "a")))

   ;; vector-assoc test
   (check-false (vector-assoc 2 (vector)))
   (check-false (vector-assoc 2 (vector (list 1) (cons 1 2) (list 3 1))))
   (check-false (vector-assoc 2 (vector "hello" 'world (cons 'random 'pairs))))
   (check-equal? (vector-assoc 1 (vector 1 2 "test" (list 3 2) (cons 1 2))) (cons 1 2))
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
                 (cons 4 1) "vector-assoc test")

   ;; cached-assoc tests
   (check-true (procedure? (cached-assoc (list (cons 1 2)) 1)))
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")

   ;; while-less test
   ;; (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
