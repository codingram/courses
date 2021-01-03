#lang htdp/isl

;; photos-starter.rkt

;; =================
;; Data definitions:

(define-struct photo (location album favourite))
;; Photo is (make-photo String String Boolean)
;; interp. a photo having a location, belonging to an album and having a
;; favourite status (true if photo is a favourite, false otherwise)
(define PHT1 (make-photo "photos/2012/june" "Victoria" true))
(define PHT2 (make-photo "photos/2013/birthday" "Birthday" true))
(define PHT3 (make-photo "photos/2012/august" "Seattle" true))

;; =================
;; Functions:


;; PROBLEM:
;;
;; Design a function called to-frame that consumes an album name and a list of photos
;; and produces a list of only those photos that are favourites and that belong to
;; the given album. You must use built-in abstract functions wherever possible.

;; String (listof Photo) -> (listof Photo)
;; Produces a list of photos that are favourites and that belong to the given album

(define (to-frame album photos)
  (local
    [(define (in-fav-album? photo)
       (and (photo-favourite photo) (string=? (photo-album photo) album)))]
    (filter in-fav-album? photos)))


;; Tests:

(check-expect (to-frame "Birthday" empty) empty)
(check-expect (to-frame "Victoria" (list PHT1 PHT2)) (list PHT1))
(check-expect (to-frame "Seattle" (list PHT1 PHT2 PHT3)) (list PHT3))

;; Alternate way of writing the list using quasiquote and unquote:
;;
;; `(,PHT1 ,PHT2 ,PHT3)
;;
;; We are using quasiquote (`) to make a list in which there are expressions which needs
;; to be evaluate before building the list. These expressions are escaped using unquote (,)
;; right before the expression. Quote (') is used only when the elements are literal values
;; and not variables or expressions.
