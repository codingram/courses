#lang racket

(require "hw5.rkt" rackunit rackunit/text-ui)

(define tests
  (test-suite
    "Assignment 5 tests"

    (test-case
      "racketlist->mupllist tests"
      (check-equal? (racketlist->mupllist (list)) (aunit))
      (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (aunit)))
      (check-equal? (racketlist->mupllist (list (int 3) (int 4)))
                    (apair (int 3) (apair (int 4) (aunit)))))

    (test-case
      "mupllist->racketlist tests"
      (check-equal? (mupllist->racketlist (aunit)) (list))
      (check-equal? (mupllist->racketlist (apair (int 3) (aunit))) (list (int 3)))
      (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
                    (list (int 3) (int 4))))

    (test-case
      "eval-exp int evaluation tests"
      (check-equal? (eval-exp (int 3)) (int 3)))

    (test-case
      "eval-exp var evaluation tests"
      (check-exn exn:fail? (lambda () (eval-under-env (var "a") null)))
      (check-equal? (eval-under-env (var "a") (list (cons "a" (int 4)))) (int 4)))

    (test-case
      "eval-exp aunit evaluation tests"
      (check-equal? (eval-exp (aunit)) (aunit)))

    (test-case
      "eval-exp isaunit evaluation tests"
      (check-equal? (eval-exp (isaunit (int 3))) (int 0))
      (check-equal? (eval-exp (isaunit (aunit))) (int 1))
      (check-equal? (eval-under-env (isaunit (var "a")) (list (cons "a" (aunit))))
                    (int 1)))

    (test-case
      "eval-exp apair evaluation tests"
      (check-equal? (eval-exp (apair (int 3) (int 4))) (apair (int 3) (int 4)))
      (check-equal? (eval-under-env (apair (var "a") (apair (int 3) (aunit)))
                                    (list (cons "a" (int 1))))
                    (apair (int 1) (apair (int 3) (aunit)))))

    (test-case
      "eval-exp fst evaluation tests"
      (check-exn exn:fail? (lambda () (eval-exp (fst (int 3)))))
      (check-equal? (eval-exp (fst (apair (int 1) (aunit)))) (int 1))
      (check-equal? (eval-exp (fst (apair (apair (int 1) (aunit)) (aunit))))
                    (apair (int 1) (aunit))))

    (test-case
      "eval-exp snd evaluation tests"
      (check-exn exn:fail? (lambda () (eval-exp (snd (aunit)))))
      (check-equal? (eval-exp (snd (apair (int 1) (aunit)))) (aunit))
      (check-equal? (eval-exp (snd (apair (int 1) (apair (int 2) (aunit)))))
                    (apair (int 2) (aunit))))

    (test-case
      "eval-exp add evaluation tests"
      (check-exn exn:fail? (lambda () (eval-exp (add (int 4) (apair (int 3) (aunit))))))
      (check-equal? (eval-exp (add (int 3) (int 3))) (int 6))
      (check-equal? (eval-under-env (add (var "a") (var "b"))
                                    (list (cons "a" (int 2)) (cons "b" (int 2))))
                    (int 4))
      (check-equal? (eval-exp (add (fst (apair (int 2) (int 10)))
                                   (snd (apair (int 2) (int 10)))))
                    (int 12)))

    (test-case
      "eval-exp ifgreater evaluation tests"
      (check-exn exn:fail? (lambda () (eval-exp (ifgreater (int 4) (aunit) (int 1) (int 0)))))
      (check-exn exn:fail? (lambda () (eval-exp (ifgreater (aunit) (int 4) (int 1) (int 0)))))
      (check-equal? (eval-exp (ifgreater (int 4) (int 2) (int 1) (int 0))) (int 1))
      (check-equal? (eval-exp (ifgreater (int 2) (int 4) (int 1) (int 0))) (int 0)))

    (test-case
      "eval-exp mlet evaluation tests"
      (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6))
      (check-equal? (eval-exp (mlet "x" (int 1)
                                    (mlet "y" (int 2) (add (var "x") (var "y")))))
                    (int 3)))

    (test-case
      "eval-exp closure evaluation tests"
      (check-equal? (eval-exp (closure '() (fun #f "x" (var "x"))))
                    (closure '() (fun #f "x" (var "x")))))

    (test-case
      "eval-exp fun evaluation tests"
      (check-equal? (eval-exp (fun #f "t" (add (var "t") (int 1))))
                    (closure '() (fun #f "t" (add (var "t") (int 1)))))
      (check-equal? (eval-under-env (fun #f "t" (var "t")) (list (cons "a" (int 1))))
                    (closure (list (cons "a" (int 1))) (fun #f "t" (var "t")))))

    (test-case
      "eval-exp call evaluation tests"
      (check-exn exn:fail? (lambda () (eval-exp (call (int 2) (int 2)))))
      (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8))
      (check-equal? (eval-exp (call (fun #f "x" (mlet "y" (int 2) (add (var "x") (var "y"))))
                                    (int 2)))
                    (int 4))
      (check-equal? (eval-under-env (call (fun #f "x" (add (var "x") (var "y")))
                                          (int 2))
                                    (list (cons "y" (int 2))))
                    (int 4))
      (check-equal? (eval-under-env (call (fun #f "x" (mlet "y" (int 10)
                                                            (add (var "x") (var "y"))))
                                          (int 2))
                                    (list (cons "y" (int 2))))
                    (int 12))
      (check-equal? (eval-under-env (call (fun #f "x" (add (var "y")
                                                           (mlet "y" (int 10)
                                                                 (add (var "x") (var "y")))))
                                          (int 2))
                                    (list (cons "y" (int 2))))
                    (int 14))
      (check-equal? (eval-under-env
                      (call (fun "f" "x"
                                 (ifgreater (var "x") (var "y")
                                            (aunit)
                                            (apair (var "x")
                                                   (call (var "f") (add (var "x") (int 1))))))
                            (int 3))
                      (list (cons "y" (int 5))))
                    (apair (int 3) (apair (int 4) (apair (int 5) (aunit)))))
      (check-equal? (eval-exp
                      (call (call (fun "o" "x"
                                       (fun "i" "y"
                                            (add (var "x") (var "y"))))
                                  (int 5))
                            (int 5)))
                    (int 10)))

    (test-case
      "ifaunit tests"
      (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3))
      (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2))
      (check-equal? (eval-exp (ifaunit (mlet "x" (aunit) (var "x")) (int 2) (int 3)))
                    (int 2))
      (check-equal? (eval-exp (ifaunit (add (int -1) (int 1)) (int 2) (int 3)))
                    (int 3)))

    (test-case
      "mlet* tests"
      (check-equal? (eval-exp (mlet* (list) (int 3))) (int 3))
      (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10))
      (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20)))
                                     (add (var "x") (var "y"))))
                    (int 30))
      (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))
                                           (cons "y" (var "x"))
                                           (cons "z" (add (var "x") (var "y"))))
                                     (add (var "x") (add (var "y") (var "z")))))
                    (int 40)))

    (test-case
      "ifeq tests"
      (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4))
      (check-equal? (eval-exp (ifeq (int 2) (int 1) (int 3) (int 4))) (int 4))
      (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3)))

    (test-case
      "mupl-map tests"
      (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 1))))
                                    (apair (int 1) (apair (int 2) (aunit)))))
                    (apair (int 2) (apair (int 3) (aunit)))))

    (test-case
      "mupl-mapAddN tests"
      (check-equal? (eval-exp (call (call mupl-mapAddN (int 4))
                                    (apair (int 1) (apair (int 2) (aunit)))))
                    (apair (int 5) (apair (int 6) (aunit)))))

    (test-case
      "combined test"
      (check-equal? (mupllist->racketlist
                      (eval-exp (call (call mupl-mapAddN (int 7))
                                      (racketlist->mupllist
                                        (list (int 3) (int 4) (int 9))))))
                    (list (int 10) (int 11) (int 16))))))


(run-tests tests)
