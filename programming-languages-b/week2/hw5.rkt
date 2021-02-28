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
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
