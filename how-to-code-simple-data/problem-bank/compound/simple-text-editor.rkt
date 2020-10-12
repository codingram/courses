#lang htdp/bsl

#| In this problem, you will be designing a simple one-line text editor.
 |
 | The constants and data definitions are provided for you, so make sure
 | to take a look through them after completing your own Domain Analysis.
 |
 | Your text editor should have the following functionality:
 | - when you type, characters should be inserted on the left side of the cursor
 | - when you press the left and right arrow keys, the cursor should move accordingly
 | - when you press backspace (or delete on a mac), the last character on the left of
 |   the cursors should be deleted |#


(require 2htdp/image)
(require 2htdp/universe)


; Constants

(define WIDTH 300)
(define HEIGHT 20)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))
(define CURSOR (rectangle 2 14 "solid" "black"))
(define TEXT-SIZE 14)
(define TEXT-COLOR "black")


; Data Definitions

(define-struct editor (pre post))
; Editor is (make-editor String String)
; interp. pre is the text before the cursor, post is the text after

; Position is Natural[0, 1]
; interp. position of the character to pop from string in string-pop function
; where 0: remove the last character
;       1: remove the first character
; for string-char function:
;       0: return the last character
;       1: return the first character

; Function definitions

; Editor -> Editor
; main function of the program, starts with (main (make-editor "" ""))

(define (main ed)
  (big-bang
    ed                         ; Editor
    (to-draw render-input)     ; Editor -> Image
    (on-key handle-input)))    ; Editor KeyEvent -> Editor

; Editor -> Image
; Produces the image of the cumulative input on BACKGROUND

(define (render-input ed)
  (overlay/align "left" "middle"
    (beside (text (editor-pre ed) TEXT-SIZE TEXT-COLOR)
            CURSOR
            (text (editor-post ed) TEXT-SIZE TEXT-COLOR))
    BACKGROUND))

; String Position -> String
; Removes either the first or last character in a given string
; according to the pos value (0 for last, 1 for first)

(define (string-pop str pos)
  (if (> (string-length str) 0)
    (substring str pos (+ (- (string-length str) 1) pos))
    str))

; String -> String
; Returns the last character of the given string

(define (string-char str pos)
  (if (> (string-length str) 0)
    (cond [(= pos 0)
           (string-ith str (- (string-length str) 1))]
          [(= pos 1)
           (string-ith str 0)])
    str))

; Editor KeyEvent -> Editor
; Handles the key presses as mentioned in the problem

(define (handle-input ed ke)
  (cond [(key=? ke "\b")
         (make-editor (string-pop (editor-pre ed) 0) (editor-post ed))]
        [(key=? ke "left")
         (make-editor (string-pop (editor-pre ed) 0)
                      (string-append (string-char (editor-pre ed) 0)
                                     (editor-post ed)))]
        [(key=? ke "right")
         (make-editor (string-append (editor-pre ed)
                                     (string-char (editor-post ed) 1))
                      (string-pop (editor-post ed) 1))]
        [(= (string-length ke) 1)
         (make-editor (string-append (editor-pre ed) ke)
                      (editor-post ed))]
        ))


; Tests

(define E1 (make-editor "" ""))           ; text input
(define E2 (make-editor "" "abc"))        ; right arrow key
(define E3 (make-editor "test!" ""))      ; backspace
(define E4 (make-editor "abc" ""))        ; left arrow key
(define E5 (make-editor "final" "test"))  ; check render

(check-expect (string-pop "hello!" 0) "hello")
(check-expect (string-pop "" 0) "")
(check-expect (string-pop "space " 0) "space")
(check-expect (string-pop "?why?" 1) "why?")
(check-expect (string-pop "" 1) "")
(check-expect (string-pop " space" 1) "space")

(check-expect (string-char "hello" 0) "o")
(check-expect (string-char "" 0) "")
(check-expect (string-char "space " 0) " ")
(check-expect (string-char "hello" 1) "h")
(check-expect (string-char "" 1) "")
(check-expect (string-char " space" 1) " ")

(check-expect (handle-input E1 "a") (make-editor "a" ""))
(check-expect (handle-input E2 "right") (make-editor "a" "bc"))
(check-expect (handle-input E3 "\b") (make-editor "test" ""))
(check-expect (handle-input E4 "left") (make-editor "ab" "c"))
(check-expect (handle-input E1 "\b") (make-editor "" ""))

(check-expect (render-input E1) (overlay/align
                                  "left" "middle"
                                  (beside (text "" TEXT-SIZE TEXT-COLOR)
                                          CURSOR
                                          (text "" TEXT-SIZE TEXT-COLOR))
                                  BACKGROUND))
(check-expect (render-input E5) (overlay/align
                                  "left" "middle"
                                  (beside (text "final" TEXT-SIZE TEXT-COLOR)
                                          CURSOR
                                          (text "test" TEXT-SIZE TEXT-COLOR))
                                  BACKGROUND))

; Uncomment the below line to run the program
; (main (make-editor "" ""))
