;; Programming Languages, Homework 5

#lang racket

(provide (all-defined-out))

;; Definition of structures for MUPL programs
(struct var (string) #:transparent)              ;; a variable, e.g., (var "foo")
(struct int (num) #:transparent)                 ;; a constant number, e.g., (int 17)
(struct add (e1 e2) #:transparent)               ;; add two expressions
(struct ifgreater (e1 e2 e3 e4) #:transparent)   ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual) #:transparent)      ;; function call
(struct mlet (var e body) #:transparent)         ;; a local binding (let var = e in body)
(struct apair (e1 e2) #:transparent)             ;; make a new pair
(struct fst (e) #:transparent)                   ;; get first part of a pair
(struct snd (e) #:transparent)                   ;; get second part of a pair
(struct aunit () #:transparent)                  ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent)               ;; evaluate to 1 if e is unit else 0

;; A closure is not in "source" programs but /is/ a MUPL value; it is what
;; functions evaluate to
(struct closure (env fun) #:transparent)



;; Problem 1: Warm-up
;; ------------------

;; Returns an analogous MUPL list with the same elements in the same order as
;; for the given Racket list
(define (racketlist->mupllist rl)
  (if (null? rl)
      (aunit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))))


;; Returns an analogous Racket list with the same elements in the same order as
;; for the given MUPL list
(define (mupllist->racketlist ml)
  (if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))



;; Problem 2: Implementing the MUPL Language
;; -----------------------------------------

;; Lookup a variable in an environment
(define (envlookup env var)
  (cond [(null? env) (error "NameError: Unbound variable during evaluation:" var)]
        [(equal? (caar env) var) (cdar env)]
        [#t (envlookup (cdr env) var)]))


;; Interpreter for MUPL under the given environment
;; NOTE: "In real life", this would be a helper function of eval-exp but as
;; this is going to be tested by the auto-grader, so it is kept in the global
;; environment.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(var? e) (envlookup env (var-string e))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "ValueError: MUPL fst applied to non-apair: " e)))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "ValueError: MUPL snd applied to non-apair: " e)))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "ValueError: MUPL addition applied to non-number: " e)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "ValueError: MUPL ifgreater applied to non-number: " e)))]
        [(mlet? e)
         (let ([ext-env (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env)])
           (eval-under-env (mlet-body e) ext-env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [arg-val (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([function (closure-fun v1)]
                      [arg-name (fun-formal function)]
                      [fun-name (fun-nameopt function)]
                      [ext-env (cons (cons arg-name arg-val) (closure-env v1))])
                 (eval-under-env (fun-body function)
                                 (if fun-name
                                     (cons (cons fun-name v1) ext-env)
                                      ext-env)))
               (error "ValueError: Invalid function call: " e)))]
        [else (error (format "SyntaxError: bad MUPL expression: ~v" e))]))


;; Interpreter for MUPL
(define (eval-exp e)
  (eval-under-env e null))



;; Problem 3: Expanding the Language
;; ---------------------------------

;; Returns a MUPL expression that when run evaluates e1 and if the result is
;; MUPL's aunit then it evaluates e2, otherwise it evaluates e3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))


;; Returns a MUPL expression that when run behaves as that of Racket's let*
(define (mlet* plst e2)
  (if (null? plst)
      e2
      (mlet (caar plst) (cdar plst) (mlet* (cdr plst) e2))))


;; Returns a MUPL expression that will evaluate e3 if and only if e1 and e2
;; are equal integers, otherwise it evaluates e4
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))



;; Problem 4: Using the Language
;; -----------------------------


;; Returns a MUPL function that acts like map.
;; It takes a MUPL function and returns a MUPL function that takes a MUPL list
;; and applies the function to every element of the list returning a new
;; MUPL list.
(define mupl-map
  (fun "f1" "f"
       (fun "f2" "ml"
            (ifaunit (var "ml")
                     (aunit)
                     (apair (call (var "f") (fst (var "ml")))
                            (call (var "f2") (snd (var "ml"))))))))


;; Takes in a MUPL integer 'i' and returns a MUPL function that takes a MUPL list
;; of MUPL integers and returns a new MUPL list of MUPL integers that adds 'i'
;; to every element of the list.
(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun "f1" "i"
             (fun "f2" "ml"
                  (call (call (var "map") (fun #f "j" (add (var "j") (var "i"))))
                        (var "ml"))))))



;; Challenge Problem
;; -----------------

;; A recursive(?) 1-argument function containing free variables
(struct fun-challenge (nameopt formal body freevars) #:transparent)

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct res (e fvs)) ; result type of f (could also use a pair)
  (define (f e)
    (cond [(var? e)
           (res e (set (var-string e)))]
          [(int? e)
           (res e (set))]
          [(add? e)
           (let ([r1 (f (add-e1 e))]
                 [r2 (f (add-e2 e))])
             (res (add (res-e r1) (res-e r2))
                  (set-union (res-fvs r1) (res-fvs r2))))]
          [(ifgreater? e)
           (let ([r1 (f (ifgreater-e1 e))]
                 [r2 (f (ifgreater-e2 e))]
                 [r3 (f (ifgreater-e3 e))]
                 [r4 (f (ifgreater-e4 e))])
             (res (ifgreater (res-e r1) (res-e r2) (res-e r3)    (res-e r4))
                  (set-union (res-fvs r1) (res-fvs r2) (res-fvs   r3) (res-fvs r4))))]
          [(fun? e)
           (let* ([r (f (fun-body e))]
                  [fvs (set-remove (res-fvs r) (fun-formal e))]
                  [fvs (if (fun-nameopt e)
                         (set-remove fvs (fun-nameopt e))
                         fvs)])
             (res (fun-challenge (fun-nameopt e) (fun-formal e)
                                 (res-e r) fvs)
                  fvs))]
          [(call? e)
           (let ([r1 (f (call-funexp e))]
                 [r2 (f (call-actual e))])
             (res (call (res-e r1) (res-e r2))
                  (set-union (res-fvs r1) (res-fvs r2))))]
          [(mlet? e)
           (let* ([r1 (f (mlet-e e))]
                  [r2 (f (mlet-body e))])
             (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                  (set-union (res-fvs r1) (set-remove (res-fvs r2)   (mlet-var e)))))]
          [(apair? e)
           (let ([r1 (f (apair-e1 e))]
                 [r2 (f (apair-e2 e))])
             (res (apair (res-e r1) (res-e r2))
                  (set-union (res-fvs r1) (res-fvs r2))))]
          [(fst? e)
           (let ([r (f (fst-e e))])
             (res (fst (res-e r))
                  (res-fvs r)))]
          [(snd? e)
           (let ([r (f (snd-e e))])
             (res (snd (res-e r))
                  (res-fvs r)))]
          [(aunit? e)
           (res e (set))]
          [(isaunit? e)
           (let ([r (f (isaunit-e e))])
             (res (isaunit (res-e r))
                  (res-fvs r)))]))
  (res-e (f e)))


(define (eval-under-env-c e env)
  (cond
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)]
         ; call case uses fun-challenge as appropriate
         ; all other cases the same
        ...))


(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))



;; Visitor pattern
;; ---------------


(define (node-visitor node env)
  (letrec
    ([visit-int
       (lambda (node)
         node)]

     [visit-var
       (lambda (node)
         (envlookup env (var-string node)))]

     [visit-fun
       (lambda (node)
         (closure env node))]

     [visit-closure
       (lambda (node)
         node)]

     [visit-aunit
       (lambda (node)
         node)]

     [visit-isaunit
       (lambda (node)
         (if (aunit? (node-visitor (isaunit-e node) env))
           (int 1)
           (int 0)))]

     [visit-apair
       (lambda (node)
         (apair (node-visitor (apair-e1 node) env)
                (node-visitor (apair-e2 node) env)))]

     [visit-fst
       (lambda (node)
         (let ([val (node-visitor (fst-e node) env)])
           (if (apair? val)
             (apair-e1 val)
             (error "ValueError: MUPL fst applied to non-apair: " node))))]

     [visit-snd
       (lambda (node)
         (let ([val (node-visitor (snd-e node) env)])
           (if (apair? val)
               (apair-e2 val)
               (error "ValueError: MUPL snd applied to non-apair: " node))))]

     [visit-add
       (lambda (node)
         (let ([val1 (node-visitor (add-e1 node) env)]
               [val2 (node-visitor (add-e2 node) env)])
           (if (and (int? val1) (int? val2))
               (int (+ (int-num val1) (int-num val2)))
               (error "ValueError: MUPL addition applied to non-number: " node))))]


     [visit-ifgreater
       (lambda (node)
         (let ([val1 (node-visitor (ifgreater-e1 node) env)]
               [val2 (node-visitor (ifgreater-e2 node) env)])
           (if (and (int? val1) (int? val2))
               (if (> (int-num val1) (int-num val2))
                   (node-visitor (ifgreater-e3 node) env)
                   (node-visitor (ifgreater-e4 node) env))
               (error "ValueError: MUPL ifgreater applied to non-number: " node))))]

     [visit-mlet
       (lambda (node)
         (node-visitor
           (mlet-body node)
           (cons (cons (mlet-var node)
                       (node-visitor (mlet-e node) env))
                 env)))]

     [visit-call
       (lambda (node)
         (let ([val1 (node-visitor (call-funexp node) env)]
               [arg-val (node-visitor (call-actual node) env)])
           (if (closure? val1)
               (let* ([function (closure-fun val1)]
                      [arg-name (fun-formal function)]
                      [fun-name (fun-nameopt function)]
                      [ext-env (cons (cons arg-name arg-val) (closure-env val1))])
                 (node-visitor (fun-body function)
                                 (if fun-name
                                     (cons (cons fun-name val1) ext-env)
                                      ext-env)))
               (error "ValueError: Invalid function call: " node))))])

    (cond [(int? node) (visit-int node)]
          [(var? node) (visit-var node)]
          [(fun? node) (visit-fun node)]
          [(closure? node) (visit-closure node)]
          [(aunit? node) (visit-aunit node)]
          [(isaunit? node) (visit-isaunit node)]
          [(apair? node) (visit-apair node)]
          [(fst? node) (visit-fst node)]
          [(snd? node) (visit-snd node)]
          [(add? node) (visit-add node)]
          [(ifgreater? node) (visit-ifgreater node)]
          [(mlet? node) (visit-mlet node)]
          [(call? node) (visit-call node)]
          [else (error (format "SyntaxError: bad MUPL expression: ~v" node))])))
