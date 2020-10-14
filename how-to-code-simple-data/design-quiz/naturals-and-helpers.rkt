#lang htdp/bsl

(require 2htdp/image)
(require "images/cookies.rkt")

; Constants

; COOKIES (from images/cookies.rkt)


; Data Definitions

; Natural is one of:
;  - 0
;  - (add1 Natural)
; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful
              (fn-for-natural (sub1 n)))]))

; Template rules used:
;  - one-of: two cases
;  - atomic distinct: 0
;  - compound: 2 fields
;  - self-reference: (sub1 n) is Natural



#| PROBLEM 1:
 |
 | Complete the design of a function called pyramid that takes a natural
 | number n and an image, and constructs an n-tall, n-wide pyramid of
 | copies of that image.
 |
 | For instance, a 3-wide pyramid of cookies would look like this:
 | Run the program to look at the image |#

; Natural Image -> Image
; produce an n-wide pyramid of the given image

(define (pyramid num img)
  (cond [(zero? num) empty-image]
        [else
         (above
           (pyramid (sub1 num) img)
           (row-images num img))]))

; Natural Image -> Image
; Produces n images in a row

(define (row-images num img)
  (cond [(zero? num) empty-image]
        [else
         (beside img
                 (row-images (sub1 num) img))]))


; Tests

(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

(check-expect (row-images 0 COOKIES) empty-image)
(check-expect (row-images 1 COOKIES) COOKIES)
(check-expect (row-images 2 COOKIES) (beside COOKIES COOKIES))


#| Problem 2:
 | Consider a test tube filled with solid blobs and bubbles.  Over time the
 | solids sink to the bottom of the test tube, and as a consequence the bubbles
 | percolate to the top.  Let's capture this idea in BSL.
 |
 | Complete the design of a function that takes a list of blobs and sinks each
 | solid blob by one. It's okay to assume that a solid blob sinks past any
 | neighbor just below it.
 |
 | To assist you, we supply the relevant data definitions. |#

; Blob is one of:
; - "solid"
; - "bubble"
; interp.  a gelatinous blob, either a solid or a bubble
; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

; Template rules used:
; - one-of: 2 cases
; - atomic distinct: "solid"
; - atomic distinct: "bubble"


; ListOfBlob is one of:
; - empty
; - (cons Blob ListOfBlob)
; interp. a sequence of blobs in a test tube, listed from top to bottom.

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

; Template rules used
; - one-of: 2 cases
; - atomic distinct: empty
; - compound: 2 fields
; - reference: (first lob) is Blob
; - self-reference: (rest lob) is ListOfBlob

; ListOfBlob -> ListOfBlob
; produce a list of blobs that sinks the given solid blobs by one

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (sink-one (first lob)
              (sink (rest lob)))]))

; Blob ListOfBlob -> ListOfBlob
; Sink one blob in the list of blobs
; Assumption: ListOfBlob is already sinked (all solids are at the end)

(define (sink-one b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if (blob-solid? b)
           (cons (first lob) (cons b (rest lob)))
           (cons b lob))]))

; Blob -> Boolean
; Produces true if the blob is solid

(define (blob-solid? b)
  (string=? b "solid"))

; Tests

(define B "bubble")
(define S "solid")

(define LOB0 empty)
(define LOB1 (cons B (cons S (cons B empty))))
(define LOB2 (cons S (cons S (cons B empty))))
(define LOB3 (cons S (cons B (cons B empty))))
(define LOB4 (cons S (cons B (cons S empty))))
(define LOB5 (cons B (cons S (cons S empty))))
(define LOB6 (cons S (cons S (cons B (cons B empty)))))
(define LOB7 (cons S (cons B (cons S (cons B (cons S (cons B empty)))))))

(check-expect (blob-solid? B) false)
(check-expect (blob-solid? S) true)

(check-expect (sink-one B LOB0) (cons B empty))
(check-expect (sink-one S LOB0) (cons S empty))
(check-expect (sink-one B LOB5) (cons B (cons B (cons S (cons S empty)))))
(check-expect (sink-one S LOB5) (cons B (cons S (cons S (cons S empty)))))

(check-expect (sink LOB0) empty)
(check-expect (sink LOB1)
              (cons B (cons B (cons S empty))))
(check-expect (sink LOB2)
              (cons B (cons S (cons S empty))))
(check-expect (sink LOB3)
              (cons B (cons S (cons B empty))))
(check-expect (sink LOB4)
              (cons B (cons S (cons S empty))))
(check-expect (sink LOB5)
              (cons B (cons S (cons S empty))))
(check-expect (sink LOB6)
              (cons B (cons S (cons S (cons B empty)))))
(check-expect (sink LOB7)
              (cons B (cons S (cons B (cons S (cons B (cons S empty)))))))
