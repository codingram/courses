#lang htdp/bsl

(require 2htdp/universe)
(require 2htdp/image)

; Space Invaders

; ====================================================
; Constants:

; Background constants
(define WIDTH  300)
(define HEIGHT 500)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))

; Speeds (not velocities) in pixels per tick
(define INVADER-X-SPEED 1.5)
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)
(define INVADE-RATE 100)

; Image construction for the invader, tank and missile
(define INVADER
  (overlay/xy
    (ellipse 10 15 "outline" "blue")              ; cockpit cover
    -5 6
    (ellipse 20 10 "solid"   "blue")))            ; saucer

(define TANK
  (overlay/xy
    (overlay (ellipse 28 8 "solid" "black")       ; tread center
             (ellipse 30 10 "solid" "green"))     ; tread outline
    5 -14
    (above (rectangle 5 10 "solid" "black")       ; gun
           (rectangle 20 10 "solid" "black"))))   ; main body

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-HEIGHT (image-height MISSILE))

; TODO What is the use of this constant?
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

; Y coordinate and the width of the tank which is going to be constant
(define TANK-Y (- HEIGHT (/ (image-height TANK) 2)))
(define TANK-WIDTH (image-width TANK))

(define INVADER-WIDTH (image-width INVADER))
(define INVADER-HEIGHT (image-height INVADER))

; ====================================================
; Data Definitions:

(define-struct game-state (invaders missiles tank))
; GameState is (make-game-state  (listof Invader) (listof Missile) Tank)
; interp. the current state of a space invaders game
;         with the current invaders, missiles and tank position
;
; Game constants defined below Missile data definition

; Template for the functions using the game-state struct
#;
(define (fn-for-game game-state)
  (... (fn-for-list-of-invader (game-state-invaders game-state))
       (fn-for-list-of-missile (game-state-missiles game-state))
       (fn-for-tank (game-state-tank game-state))))


(define-struct tank (x dir))
; Tank is (make-tank Number Integer[-1, 1])
; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1,
;         and stays at the current position if it is 0 (usually useful only at the start
;         of the game).


#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
; Invader is (make-invader Number Number Number)
; interp. the invader is at (x, y) in screen coordinates
;         the invader along x by dx pixels per clock tick


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
; Missile is (make-missile Number Number)
; interp. the missile's location is x y in screen coordinates

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


; ====================================================
; Functions:

; GameState -> GameState
; Main function of the program; starts with (main G0)

(define (main gs)
  (big-bang gs                               ; GameState
     (on-tick next-state)                    ; GameState -> GameState
     (to-draw render-state)                  ; GameState -> Image
     (on-key handle-key)                     ; GameState key -> GameState
     (stop-when game-over? last-image)))     ; GameState -> Boolean, GameState -> Image


; ====================================================
; Produce the next state of the game.

; GameState -> GameState
; Produces the next state of the game by producing/advancing invaders and/or missile
; and/or tank

(define (next-state game-state)
  (cond [(< (random INVADE-RATE) 2)
         (make-game-state
           (append (next-invaders (game-state-invaders game-state)
                                  (game-state-missiles game-state))
                   (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))
           (next-missiles (game-state-missiles game-state)
                          (game-state-invaders game-state))
           (next-tank (game-state-tank game-state)))]
        [else
          (make-game-state (next-invaders (game-state-invaders game-state)
                                          (game-state-missiles game-state))
                           (next-missiles (game-state-missiles game-state)
                                          (game-state-invaders game-state))
                           (next-tank (game-state-tank game-state)))]))

; ListOfInvaders ListOfMissiles -> ListOfInvaders
; Move all the invaders with the provided X and Y speed and remove the ones which got
; hit by any of the given missiles.

(define (next-invaders list-of-invaders list-of-missiles)
  (move-invaders (filter-invaders list-of-invaders list-of-missiles)))


; ListOfInvaders ListOfMissiles -> ListOfInvaders
; Filter out any of the invaders which got hit by any of the given missiles.

(define (filter-invaders list-of-invaders list-of-missiles)
  (cond [(empty? list-of-invaders) empty]
        [(is-invader-hit? (first list-of-invaders) list-of-missiles)
         (filter-invaders (rest list-of-invaders) list-of-missiles)]
        [else
          (append (filter-invaders (rest list-of-invaders) list-of-missiles)
                  (list (first list-of-invaders)))]))


; Invader ListOfMissiles -> Boolean
; Determine whether the given invader is hit by any of the given missiles.

(define (is-invader-hit? invader list-of-missiles)
  (cond [(empty? list-of-missiles) false]
        [(invader-hit-missile? invader (first list-of-missiles)) true]
        [else (is-invader-hit? invader (rest list-of-missiles))]))


; Invader Missile -> Boolean
; Determine whether the given invader hits the provided missile.

(define (invader-hit-missile? invader missile)
  (and
    (<= (- (invader-x invader) HIT-RANGE)
        (missile-x missile)
        (+ (invader-x invader) HIT-RANGE))
    (<= (- (invader-y invader) HIT-RANGE)
        (missile-y missile)
        (+ (invader-y invader) HIT-RANGE))))


; ListOfInvaders -> ListOfInvaders
; Move all the invaders with the respective X and Y speed, changing the direction of
; X axis if any of the invaders reaches the boundary.

(define (move-invaders list-of-invaders)
  (cond [(empty? list-of-invaders) empty]
        [else
          (append (move-invaders (rest list-of-invaders))
                  (list (move-single-invader (first list-of-invaders))))]))


; Invader -> Invader
; Move a single invader with the respective X and Y speed.

(define (move-single-invader invader)
  (cond [(or (> (+ (invader-x invader) (invader-dx invader))
                (- WIDTH (/ INVADER-WIDTH 2)))
             (< (+ (invader-x invader) (invader-dx invader))
                (/ INVADER-WIDTH 2)))
         (make-invader (invader-x invader) (invader-y invader) (- (invader-dx invader)))]
        [else
          (make-invader (+ (invader-x invader) (invader-dx invader))
                        (+ (invader-y invader) INVADER-Y-SPEED)
                        (invader-dx invader))]))


; ListOfMissiles ListOfInvaders -> ListOfMissiles
; Move all the missile with MISSILE-SPEED and remove all the missiles which are outside
; the BACKGROUND and which are within the hit range of any one of the given invaders.

(define (next-missiles list-of-missiles list-of-invaders)
  (move-missiles (filter-missiles list-of-missiles list-of-invaders)))


; ListOfMissiles ListOfInvaders -> ListOfMissiles
; Helper function to filter out all the missiles which are outside the BACKGROUND and
; which are within the hit range of any one of the given invaders.
; NOTE: Missiles which hit the invaders should be filtered out first before trying to
; filter out the missiles outside the BACKGROUND.

(define (filter-missiles list-of-missiles list-of-invaders )
  (filter-outside-missiles (filter-hit-missiles list-of-missiles list-of-invaders)))


; ListOfMissiles ListOfInvaders -> ListOfMissiles
; Helper function to filter out all the missiles which are within the hit range of
; any one of the given invaders.

(define (filter-hit-missiles list-of-missiles list-of-invaders)
  (cond [(empty? list-of-missiles) empty]
        [(is-missile-hit? (first list-of-missiles) list-of-invaders)
         (filter-hit-missiles (rest list-of-missiles) list-of-invaders)]
        [else
          (append (filter-hit-missiles (rest list-of-missiles) list-of-invaders)
                  (list (first list-of-missiles)))]))


; ListOfMissiles -> ListOfMissiles
; Helper function to filter out all the missiles which are outside the BACKGROUND.

(define (filter-outside-missiles list-of-missiles)
  (cond [(empty? list-of-missiles) empty]
        [(is-missile-outside? (first list-of-missiles))
         (filter-outside-missiles (rest list-of-missiles))]
        [else
          (append (filter-outside-missiles (rest list-of-missiles))
                  (list (first list-of-missiles)))]))


; Missile -> Boolean
; Determines whether the given missile is outside the BACKGROUND

(define (is-missile-outside? missile)
  (<= (missile-y missile) (- (/ MISSILE-HEIGHT 2))))


; Missile ListOfInvaders -> Boolean
; Determines whether the given missile is within the hit range of any one of the
; given invaders. Returns True if it is, false otherwise.

(define (is-missile-hit? missile list-of-invaders)
  (cond [(empty? list-of-invaders) false]
        [(missile-hit-invader? missile (first list-of-invaders)) true]
        [else (is-missile-hit? missile (rest list-of-invaders))]))


; Missile Invader -> Boolean
; Determines whether the given missile is within the hit range of the given invader.
; Returns True if it is, false otherwise. Helper function for is-missile-hit?

(define (missile-hit-invader? missile invader)
  (and
    (<= (- (missile-x missile) HIT-RANGE)
        (invader-x invader)
        (+ (missile-x missile) HIT-RANGE))
    (<= (- (missile-y missile) HIT-RANGE)
        (invader-y invader)
        (+ (missile-y missile) HIT-RANGE))))


; ListOfMissiles -> ListOfMissiles
; Helper function to move all the missiles by MISSILE-SPEED.
; NOTE: This assumes that all the unwanted missiles (outside the BACKGROUND) have
; been filtered out of the list.

(define (move-missiles list-of-missiles)
  (cond [(empty? list-of-missiles) empty]
        [else
          (append (move-missiles (rest list-of-missiles))
                  (list (move-single-missile (first list-of-missiles))))]))


; Missile -> Missile
; Helper function to move a single missile
; NOTE: As the missile is launched from the bottom of the scene, the speed should
; be subtracted instead of added.

(define (move-single-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))


; Tank -> Tank
; Move the tank in the appropriate direction with TANK-SPEED.

(define (next-tank tank)
  (cond [(or (> (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                (- WIDTH (/ TANK-WIDTH 2)))
             (< (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                (/ TANK-WIDTH 2)))
         (make-tank (tank-x tank) (- (tank-dir tank)))]
        [else
          (make-tank (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                     (tank-dir tank))]))


; ====================================================
; Render the game state into the image. The rendering process takes place on three levels:
; - First, the tank is rendered on top of the base image which is BACKGROUND.
; - Second, all the missiles are rendered on top of the first image recursively where
;   the image of the first missile is produced and then the same process repeats recursively
;   for the rest of the missiles.
; - Third, all the invaders are rendered on top of the second image, again recursively in
;   the same manner as described in the previous point.

; GameState -> Image
; Produces the image of the current state of the game

(define (render-state game-state)
  (render-invaders (game-state-invaders game-state)
                   (render-missiles (game-state-missiles game-state)
                                    (render-tank (game-state-tank game-state) BACKGROUND))))


; ListOfInvaders Image -> Image
; Produces the image of all the invaders on top of the provided image

(define (render-invaders list-of-invaders bottom-image)
  (cond [(empty? list-of-invaders) bottom-image]
        [else
          (place-image INVADER
                       (invader-x (first list-of-invaders))
                       (invader-y (first list-of-invaders))
                       (render-invaders (rest list-of-invaders) bottom-image))]))


; ListOfMissiles Image -> Image
; Produces the image of all the missiles on top of the provided image

(define (render-missiles list-of-missiles bottom-image)
  (cond [(empty? list-of-missiles) bottom-image]
        [else
          (place-image MISSILE
                       (missile-x (first list-of-missiles))
                       (missile-y (first list-of-missiles))
                       (render-missiles (rest list-of-missiles) bottom-image))]))


; Tank Image -> Image
; Produces the image of the tank on top of the provided image

(define (render-tank tank bottom-image)
  (place-image TANK (tank-x tank) TANK-Y BACKGROUND))


; ====================================================
; Handle the three key events which are:
; - When a spacebar is pressed, a missile from the location of the tank should be
;   released going straight up.
; - When the right arrow key is pressed, the tank should change direction from the
;   previous direction to go right.
; - When the left arrow key is pressed, the tank should change direction from the
;   previous direction to go left.

; GameState key -> GameState
; Handles the neccessary key presses and produces the game state accordingly

(define (handle-key game-state key)
  (cond [(key=? key " ") (handle-key-space game-state)]
        [(key=? key "left") (handle-key-left game-state)]
        [(key=? key "right") (handle-key-right game-state)]
        [else game-state]))


; GameState -> GameState
; Handle a specific key event where the spacebar was pressed.

(define (handle-key-space game-state)
  (make-game-state (game-state-invaders game-state)
                   (append (game-state-missiles game-state)
                           (list (make-missile
                                   (tank-x (game-state-tank game-state)) TANK-Y)))
                   (game-state-tank game-state)))


; GameState -> GameState
; Handle a specific key event where the right arrow key was pressed.

(define (handle-key-right game-state)
  (make-game-state (game-state-invaders game-state)
                   (game-state-missiles game-state)
                   (make-tank (tank-x (game-state-tank game-state)) 1)))


; GameState -> GameState
; Handle a specific key event where the left arrow key was pressed.

(define (handle-key-left game-state)
  (make-game-state (game-state-invaders game-state)
                   (game-state-missiles game-state)
                   (make-tank (tank-x (game-state-tank game-state)) -1)))


; ====================================================
; Check whether the game is over according to the given game-state. When it is
; over, produce the final image stating 'GAME OVER'

; GameState -> Boolean
; Produces True if the game is over, otherwise False

(define (game-over? game-state)
  (cond [(empty? (game-state-invaders game-state)) false]
        [(invader-touchdown? (first (game-state-invaders game-state))) true]
        [else
          (game-over? (make-game-state
                        (rest (game-state-invaders game-state))
                        (game-state-missiles game-state)
                        (game-state-tank game-state)))]))


; Invader -> Boolean
; Determine whether the provided invader made a touchdown on our base!

(define (invader-touchdown? invader)
  (>= (invader-y invader) (- HEIGHT (/ INVADER-HEIGHT 2))))

; GameState -> Image
; Produces the final image of the game with 'GAME OVER' written

(define (last-image game-state)
  (place-image
    (text "GAME OVER!" 24 "black")
    (/ WIDTH 2)
    (/ HEIGHT 2)
    (render-state game-state)))

; ====================================================
; Tests

(define T0 (make-tank (/ WIDTH 2) 0))   ; center staying where it is
(define T1 (make-tank (/ WIDTH 2) 1))   ; center going right
(define T2 (make-tank 50 1))            ; going right
(define T3 (make-tank 50 -1))           ; going left

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

; GameState where the tank is still
(define GS0 (make-game-state empty empty T0))
; GameState where the tank is moving in the right direction
(define GS1 (make-game-state empty empty T1))
(define GS2 (make-game-state (list I1) (list M1) T1))
; GameState where the tank is moving in the left direction
(define GS3 (make-game-state (list I1 I2) (list M1 M2) T1))


; Test the function which will dispatch the game state to the appropriate key
; handle functions. We have already checked the individual key handling functions
; and so we will use that to check whether the appropriate function is being called
; as per the key event.

(check-expect (handle-key GS0 " ") (handle-key-space GS0))
(check-expect (handle-key GS0 "r") GS0)
(check-expect (handle-key GS1 "left") (handle-key-left GS1))
(check-expect (handle-key GS3 "right") (handle-key-right GS3))

; Test the function which handles the space bar key pressed.
(check-expect (handle-key-space GS0)
              (make-game-state (game-state-invaders GS0)
                               (append (game-state-missiles GS0)
                                       (list (make-missile
                                               (tank-x (game-state-tank GS0)) TANK-Y)))
                               (game-state-tank GS0)))
(check-expect (handle-key-space GS1)
              (make-game-state (game-state-invaders GS1)
                               (append (game-state-missiles GS1)
                                       (list (make-missile
                                               (tank-x (game-state-tank GS1)) TANK-Y)))
                               (game-state-tank GS1)))
(check-expect (handle-key-space GS3)
              (make-game-state (game-state-invaders GS3)
                               (append (game-state-missiles GS3)
                                       (list (make-missile
                                               (tank-x (game-state-tank GS3)) TANK-Y)))
                               (game-state-tank GS3)))


; Test the function which handles the left arrow key pressed.
(check-expect (handle-key-left GS0)
              (make-game-state (game-state-invaders GS0)
                               (game-state-missiles GS0)
                               (make-tank (tank-x (game-state-tank GS0)) -1)))
(check-expect (handle-key-left GS1)
              (make-game-state (game-state-invaders GS1)
                               (game-state-missiles GS1)
                               (make-tank (tank-x (game-state-tank GS1)) -1)))
(check-expect (handle-key-left GS3)
              (make-game-state (game-state-invaders GS3)
                               (game-state-missiles GS3)
                               (make-tank (tank-x (game-state-tank GS3)) -1)))

; Test the function which handles the right arrow key pressed.
(check-expect (handle-key-right GS0)
              (make-game-state (game-state-invaders GS0)
                               (game-state-missiles GS0)
                               (make-tank (tank-x (game-state-tank GS0)) 1)))
(check-expect (handle-key-right GS1)
              (make-game-state (game-state-invaders GS1)
                               (game-state-missiles GS1)
                               (make-tank (tank-x (game-state-tank GS1)) 1)))
(check-expect (handle-key-right GS3)
              (make-game-state (game-state-invaders GS3)
                               (game-state-missiles GS3)
                               (make-tank (tank-x (game-state-tank GS3)) 1)))


; Uncomment the below line to start the game!
; Initial state of the game as used in the main function.
(define INITIAL-GAME-STATE (make-game-state empty empty (make-tank (/ WIDTH 2) 0)))
(main INITIAL-GAME-STATE)
