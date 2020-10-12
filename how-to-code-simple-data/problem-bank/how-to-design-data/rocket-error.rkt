;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket-error) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This is an external library, download from the terminal:
;; $ raco pkg install https://github.com/GregorKiczales/spd.git
(require spd/tags)

(@Problem 1)
;; Consider the following data definition from the Rocket practice problem.
;;
;; We have designed a function has-landed?, but there are errors in the function
;; design. Uncomment the program below, and make the minimal changes possible to
;; a) make this program work properly and b) make the function design
;; consistent.


;; =================
;; Data Definitions: 

(@HtDD RocketDescent)
;; RocketDescent is one of:
;; - Number(0, 100]
;; - false
;; interp. false if rocket's descent has ended, otherwise number of kilometers
;;         left to Earth, restricted to (0, 100]
(define RD1 100)
(define RD2 40)
(define RD3 0.5)
(define RD4 false)

(@dd-template-rules one-of              ;2 cases
                    atomic-non-distinct ;Number(0, 100]
                    atomic-distinct)    ;false
#;
(define (fn-for-rocket-descent rd)
  (cond [(and (number? rd)
              (> rd 0)
              (<= rd 100))
              (... rd)]
         [else  (...)])) 



;; =================
;; Functions:

(@HtDF has-landed?)
(@signature RocketDescent -> Boolean)
;; produce true if rocket's descent has ended; false if it's still descending
(check-expect (has-landed? 100) false)
(check-expect (has-landed? 23) false)
(check-expect (has-landed? 0.25) false)
(check-expect (has-landed? false) true)

;(define (has-landed? r) r) ; stub

(@template RocketDescent)

(define (has-landed? rd)
  (cond [(and (number? rd)
              (> rd 0)
              (<= rd 100))
              false]
         [else  true]))  