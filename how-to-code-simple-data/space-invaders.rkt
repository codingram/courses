#lang htdp/bsl

(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ====================================================
;; Constants:

;; Background constants
(define WIDTH  300)
(define HEIGHT 500)
(define BG-COLOR "white")
(define BACKGROUND (empty-scene WIDTH HEIGHT BG-COLOR))

;; Speeds (not velocities) in pixels per tick
(define INVADER-X-SPEED 1.5)
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)
(define INVADE-RATE 100)

;; Image construction for the invader, tank and missile
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

;; Some necessary information about the images to be used throughout the program
(define MISSILE-HEIGHT (image-height MISSILE))

(define TANK-Y (- HEIGHT (/ (image-height TANK) 2)))
(define TANK-WIDTH (image-width TANK))

(define INVADER-WIDTH (image-width INVADER))
(define INVADER-HEIGHT (image-height INVADER))

;; When an invader pops extremely close to the edges, it goes into the infinite
;; loop of forever changing the direction and not moving forward resulting into
;; them being stuck in the corners. This will be the max value for the random
;; function and we will add half of INVADER-WIDTH to it in case the random number
;; generated is close to the left edge.
(define WIDTH-MAX (- WIDTH INVADER-WIDTH))

;; ====================================================
;; Data Definitions:

(define-struct game-state (invaders missiles tank))
;; GameState is (make-game-state  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;;
;; Game constants defined below Missile data definition

;; Template for the functions using the game-state struct
#;
(define (fn-for-game game-state)
  (... (fn-for-list-of-invader (game-state-invaders game-state))
       (fn-for-list-of-missile (game-state-missiles game-state))
       (fn-for-tank (game-state-tank game-state))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1,
;;         and stays at the current position if it is 0 (usually useful only at the start
;;         of the game).


#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ====================================================
;; Functions:

;; GameState -> GameState
;; Main function of the program; starts with (main G0)

(define (main gs)
  (big-bang gs                               ; GameState
     (on-tick next-state)                    ; GameState -> GameState
     (to-draw render-state)                  ; GameState -> Image
     (on-key handle-key)                     ; GameState key -> GameState
     (stop-when game-over? last-image)))     ; GameState -> Boolean, GameState -> Image


;; ====================================================
;; Produce the next state of the game.

;; GameState -> GameState
;; Produces the next state of the game by producing/advancing invaders and/or missile
;; and/or tank
;; As this is a random function, there are no tests for it.

(define (next-state game-state)
  (cond [(< (random INVADE-RATE) 2)
         (make-game-state
           (append (next-invaders (game-state-invaders game-state)
                                  (game-state-missiles game-state))
                   (list (make-invader
                           (+ (random WIDTH-MAX) (/ INVADER-WIDTH 2))
                           0 INVADER-X-SPEED)))
           (next-missiles (game-state-missiles game-state)
                          (game-state-invaders game-state))
           (next-tank (game-state-tank game-state)))]
        [else
          (make-game-state
            (next-invaders (game-state-invaders game-state)
                           (game-state-missiles game-state))
            (next-missiles (game-state-missiles game-state)
                           (game-state-invaders game-state))
            (next-tank (game-state-tank game-state)))]))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Move all the invaders with the provided X and Y speed and remove the ones which got
;; hit by any of the given missiles.

(define (next-invaders list-of-invaders list-of-missiles)
  (move-invaders (filter-invaders list-of-invaders list-of-missiles)))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Filter out any of the invaders which got hit by any of the given missiles.

(define (filter-invaders list-of-invaders list-of-missiles)
  (cond [(empty? list-of-invaders) empty]
        [(is-invader-hit? (first list-of-invaders) list-of-missiles)
         (filter-invaders (rest list-of-invaders) list-of-missiles)]
        [else
          (append (list (first list-of-invaders))
                  (filter-invaders (rest list-of-invaders) list-of-missiles))]))


;; Invader ListOfMissiles -> Boolean
;; Determine whether the given invader is hit by any of the given missiles.

(define (is-invader-hit? invader list-of-missiles)
  (cond [(empty? list-of-missiles) false]
        [(invader-hit-missile? invader (first list-of-missiles)) true]
        [else (is-invader-hit? invader (rest list-of-missiles))]))


;; Invader Missile -> Boolean
;; Determine whether the given invader hits the provided missile.

(define (invader-hit-missile? invader missile)
  (and
    (<= (- (invader-x invader) HIT-RANGE)
        (missile-x missile)
        (+ (invader-x invader) HIT-RANGE))
    (<= (- (invader-y invader) HIT-RANGE)
        (missile-y missile)
        (+ (invader-y invader) HIT-RANGE))))


;; ListOfInvaders -> ListOfInvaders
;; Move all the invaders with the respective X and Y speed, changing the direction of
;; X axis if any of the invaders reaches the boundary.

(define (move-invaders list-of-invaders)
  (cond [(empty? list-of-invaders) empty]
        [else
          (append (list (move-single-invader (first list-of-invaders)))
                  (move-invaders (rest list-of-invaders)))]))


;; Invader -> Invader
;; Move a single invader with the respective X and Y speed.

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


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Move all the missile with MISSILE-SPEED and remove all the missiles which are outside
;; the BACKGROUND and which are within the hit range of any one of the given invaders.

(define (next-missiles list-of-missiles list-of-invaders)
  (move-missiles (filter-missiles list-of-missiles list-of-invaders)))


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Helper function to filter out all the missiles which are outside the BACKGROUND and
;; which are within the hit range of any one of the given invaders.
;; NOTE: Missiles which hit the invaders should be filtered out first before trying to
;; filter out the missiles outside the BACKGROUND.

(define (filter-missiles list-of-missiles list-of-invaders )
  (filter-outside-missiles (filter-hit-missiles list-of-missiles list-of-invaders)))


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Helper function to filter out all the missiles which are within the hit range of
;; any one of the given invaders.

(define (filter-hit-missiles list-of-missiles list-of-invaders)
  (cond [(empty? list-of-missiles) empty]
        [(is-missile-hit? (first list-of-missiles) list-of-invaders)
         (filter-hit-missiles (rest list-of-missiles) list-of-invaders)]
        [else
          (append (list (first list-of-missiles))
                  (filter-hit-missiles (rest list-of-missiles) list-of-invaders))]))


;; ListOfMissiles -> ListOfMissiles
;; Helper function to filter out all the missiles which are outside the BACKGROUND.

(define (filter-outside-missiles list-of-missiles)
  (cond [(empty? list-of-missiles) empty]
        [(is-missile-outside? (first list-of-missiles))
         (filter-outside-missiles (rest list-of-missiles))]
        [else
          (append (list (first list-of-missiles))
                  (filter-outside-missiles (rest list-of-missiles)))]))


;; Missile -> Boolean
;; Determines whether the given missile is outside the BACKGROUND

(define (is-missile-outside? missile)
  (<= (missile-y missile) (- (/ MISSILE-HEIGHT 2))))


;; Missile ListOfInvaders -> Boolean
;; Determines whether the given missile is within the hit range of any one of the
;; given invaders. Returns True if it is, false otherwise.

(define (is-missile-hit? missile list-of-invaders)
  (cond [(empty? list-of-invaders) false]
        [(missile-hit-invader? missile (first list-of-invaders)) true]
        [else (is-missile-hit? missile (rest list-of-invaders))]))


;; Missile Invader -> Boolean
;; Determines whether the given missile is within the hit range of the given invader.
;; Returns True if it is, false otherwise. Helper function for is-missile-hit?

(define (missile-hit-invader? missile invader)
  (and
    (<= (- (missile-x missile) HIT-RANGE)
        (invader-x invader)
        (+ (missile-x missile) HIT-RANGE))
    (<= (- (missile-y missile) HIT-RANGE)
        (invader-y invader)
        (+ (missile-y missile) HIT-RANGE))))


;; ListOfMissiles -> ListOfMissiles
;; Helper function to move all the missiles by MISSILE-SPEED.
;; NOTE: This assumes that all the unwanted missiles (outside the BACKGROUND) have
;; been filtered out of the list.

(define (move-missiles list-of-missiles)
  (cond [(empty? list-of-missiles) empty]
        [else
          (append
            (list (move-single-missile (first list-of-missiles)))
            (move-missiles (rest list-of-missiles)))]))


;; Missile -> Missile
;; Helper function to move a single missile
;; NOTE: As the missile is launched from the bottom of the scene, the speed should
;; be subtracted instead of added.

(define (move-single-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))


;; Tank -> Tank
;; Move the tank in the appropriate direction with TANK-SPEED.

(define (next-tank tank)
  (cond [(or (> (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                (- WIDTH (/ TANK-WIDTH 2)))
             (< (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                (/ TANK-WIDTH 2)))
         (make-tank (tank-x tank) (- (tank-dir tank)))]
        [else
          (make-tank (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED))
                     (tank-dir tank))]))


;; ====================================================
;; Render the game state into the image. The rendering process takes place on three levels:
;; - First, the tank is rendered on top of the base image which is BACKGROUND.
;; - Second, all the missiles are rendered on top of the first image recursively where
;;   the image of the first missile is produced and then the same process repeats recursively
;;   for the rest of the missiles.
;; - Third, all the invaders are rendered on top of the second image, again recursively in
;;   the same manner as described in the previous point.

;; GameState -> Image
;; Produces the image of the current state of the game

(define (render-state game-state)
  (render-invaders (game-state-invaders game-state)
                   (render-missiles (game-state-missiles game-state)
                                    (render-tank (game-state-tank game-state) BACKGROUND))))


;; ListOfInvaders Image -> Image
;; Produces the image of all the invaders on top of the provided image

(define (render-invaders list-of-invaders bottom-image)
  (cond [(empty? list-of-invaders) bottom-image]
        [else
          (place-image INVADER
                       (invader-x (first list-of-invaders))
                       (invader-y (first list-of-invaders))
                       (render-invaders (rest list-of-invaders) bottom-image))]))


;; ListOfMissiles Image -> Image
;; Produces the image of all the missiles on top of the provided image

(define (render-missiles list-of-missiles bottom-image)
  (cond [(empty? list-of-missiles) bottom-image]
        [else
          (place-image MISSILE
                       (missile-x (first list-of-missiles))
                       (missile-y (first list-of-missiles))
                       (render-missiles (rest list-of-missiles) bottom-image))]))


;; Tank Image -> Image
;; Produces the image of the tank on top of the provided image

(define (render-tank tank bottom-image)
  (place-image TANK (tank-x tank) TANK-Y BACKGROUND))


;; ====================================================
;; Handle the three key events which are:
;; - When a spacebar is pressed, a missile from the location of the tank should be
;;   released going straight up.
;; - When the right arrow key is pressed, the tank should change direction from the
;;   previous direction to go right.
;; - When the left arrow key is pressed, the tank should change direction from the
;;   previous direction to go left.

;; GameState key -> GameState
;; Handles the neccessary key presses and produces the game state accordingly

(define (handle-key game-state key)
  (cond [(key=? key " ") (handle-key-space game-state)]
        [(key=? key "left") (handle-key-left game-state)]
        [(key=? key "right") (handle-key-right game-state)]
        [else game-state]))


;; GameState -> GameState
;; Handle a specific key event where the spacebar was pressed.

(define (handle-key-space game-state)
  (make-game-state
    (game-state-invaders game-state)
    (append (game-state-missiles game-state)
            (list (make-missile
                  (tank-x (game-state-tank game-state)) TANK-Y)))
    (game-state-tank game-state)))


;; GameState -> GameState
;; Handle a specific key event where the right arrow key was pressed.

(define (handle-key-right game-state)
  (make-game-state
    (game-state-invaders game-state)
    (game-state-missiles game-state)
    (make-tank (tank-x (game-state-tank game-state)) 1)))


;; GameState -> GameState
;; Handle a specific key event where the left arrow key was pressed.

(define (handle-key-left game-state)
  (make-game-state
    (game-state-invaders game-state)
    (game-state-missiles game-state)
    (make-tank (tank-x (game-state-tank game-state)) -1)))


;; ====================================================
;; Check whether the game is over according to the given game-state. When it is
;; over, produce the final image stating 'GAME OVER'

;; GameState -> Boolean
;; Produces True if the game is over, otherwise False

(define (game-over? game-state)
  (cond [(empty? (game-state-invaders game-state)) false]
        [(invader-touchdown? (first (game-state-invaders game-state))) true]
        [else
          (game-over? (make-game-state
                        (rest (game-state-invaders game-state))
                        (game-state-missiles game-state)
                        (game-state-tank game-state)))]))


;; Invader -> Boolean
;; Determine whether the provided invader made a touchdown on our base!

(define (invader-touchdown? invader)
  (>= (invader-y invader) (- HEIGHT (/ INVADER-HEIGHT 2))))

;; GameState -> Image
;; Produces the final image of the game with 'GAME OVER' written

(define (last-image game-state)
  (place-image
    (text "GAME OVER!" 24 "black")
    (/ WIDTH 2)
    (/ HEIGHT 2)
    (render-state game-state)))

;; ====================================================
;; Tests

;; Center staying where it is (initial state of the tank)
(define T0 (make-tank (/ WIDTH 2) 0))
(define T1 (make-tank 50 1))                           ; going right
(define T2 (make-tank 50 -1))                          ; going left
(define T3 (make-tank (- WIDTH (/ TANK-WIDTH 2)) 1))   ; on the right edge, going right
(define T4 (make-tank (/ TANK-WIDTH 2) -1))            ; on the left edge, going left

(define I0 (make-invader 250 100 -10))                     ; not landed, moving left
(define I1 (make-invader 150 100 10))                      ; not landed, moving right
(define I2 (make-invader 150 (- HEIGHT (/ INVADER-HEIGHT 2)) -10))  ; exactly landed, moving left
(define I3 (make-invader 150 (+ (- HEIGHT (/ INVADER-HEIGHT 2)) 10) 10)) ; > landed, moving right
(define I4 (make-invader (- WIDTH (/ INVADER-WIDTH 2)) 100 10))  ; on the right edge, moving right
(define I5 (make-invader (/ INVADER-WIDTH 2) 100 -10))           ; on the left edge, moving left

(define M1 (make-missile (invader-x I1) 300))             ; missile is in range for x-axis
(define M2 (make-missile 200 (invader-y I1)))             ; missile is in range for y-axis
(define M3 (make-missile (invader-x I1) (invader-y I1)))  ; missile is in range for x and y axis
(define M4 (make-missile 100 (- (/ MISSILE-HEIGHT 2))))   ; missile is outside the BACKGROUND

;; GameState where the tank is still
(define GS0 (make-game-state empty empty T0))
;; GameState where the tank is moving in the right direction
(define GS1 (make-game-state empty empty T1))
;; I1 made the touchdown, so the game is over
(define GS2 (make-game-state (list I1) (list M1) T1))
;; GameState where the tank is moving in the left direction
(define GS3 (make-game-state (list I2 I1) (list M1 M2) T2))
;; GameState where the next tank made the touchdown
(define GS4 (make-game-state (list I0 I1 I3) (list M1 M2) T1))

;; Test whether the given invader hits the missile
(check-expect (invader-hit-missile? I0 M3) false)
(check-expect (invader-hit-missile? I1 M3) true)

;; Test the function which determines whether the invader hits any one of the missiles
(check-expect (is-invader-hit? I0 empty) false)
(check-expect (is-invader-hit? I1 (list M1 M2)) false)
(check-expect (is-invader-hit? I1 (list M2 M3)) true)

;; Test the function which filters out the invaders which got hit by any one of the missiles
(check-expect (filter-invaders empty (list M1 M2)) empty)
(check-expect (filter-invaders (list I1 I2) (list M3 M2)) (list I2))
(check-expect (filter-invaders (list I3 I2 I1) (list M2 M3)) (list I3 I2))
(check-expect (filter-invaders (list I1 I2) (list M1 M2)) (list I1 I2))

;; Test the function which moves a single invader
(check-expect (move-single-invader I0)
              (make-invader (+ (invader-x I0) (invader-dx I0))
                            (+ (invader-y I0) INVADER-Y-SPEED)
                            (invader-dx I0)))
(check-expect (move-single-invader I4)
              (make-invader (invader-x I4) (invader-y I4) (- (invader-dx I4))))
(check-expect (move-single-invader I5)
              (make-invader (invader-x I5) (invader-y I5) (- (invader-dx I5))))

;; Test the function which moves all the invaders
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1 I2))
                             (list (move-single-invader I1) (move-single-invader I2)))

;; Test the function which filters out the hit invaders and moves the rest
(check-expect (next-invaders (list I2 I1) (list M2 M3))
              (list (move-single-invader I2)))

;; Test the function which determines whether the missile is outside the BACKGROUND
(check-expect (is-missile-outside? M1) false)
(check-expect (is-missile-outside? M4) true)

;; Test the function which filters out the missiles outside the BACKGROUND
(check-expect (filter-outside-missiles empty) empty)
(check-expect (filter-outside-missiles (list M4 M1)) (list M1))
(check-expect (filter-outside-missiles (list M1 M2 M4)) (list M1 M2))
(check-expect (filter-outside-missiles (list M1 M4 M2 M4)) (list M1 M2))
(check-expect (filter-outside-missiles (list M1 M2 M3)) (list M1 M2 M3))

;; Test the function which determines whether the missile hits the given invader
(check-expect (missile-hit-invader? M1 I1) false)
(check-expect (missile-hit-invader? M2 I1) false)
(check-expect (missile-hit-invader? M3 I1) true)

;; Test the function which will determine if the given missile hit any one of the invaders
(check-expect (is-missile-hit? M1 empty) false)
(check-expect (is-missile-hit? M3 (list I1 I2)) true)
(check-expect (is-missile-hit? M3 (list I2 I1)) true)
(check-expect (is-missile-hit? M2 (list I1 I2)) false)

;; Test the function which filters out all the missiles which hit any one of the invaders
(check-expect (filter-hit-missiles empty (list I1 I2)) empty)
(check-expect (filter-hit-missiles (list M3 M1) (list I1 I2)) (list M1))
(check-expect (filter-hit-missiles (list M1 M2 M3) (list I2 I1)) (list M1 M2))
(check-expect (filter-hit-missiles (list M1 M3) (list I2 I3)) (list M1 M3))

;; Test the function which filters missiles outside BACKGROUND and which hit any one
;; of the invaders. This function only delegates the tasks to the helper functions
;; which have already been tested, so the only case remaining is the mixed one.
(check-expect (filter-missiles (list M1 M2 M4 M3) (list I1 I2)) (list M1 M2))

;; Test the function which will move a single missile
(check-expect (move-single-missile M1)
              (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))

;; Test the function which moves all the missiles
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1 M2))
              (list (move-single-missile M1) (move-single-missile M2)))

;; Test the function which filters out the missiles and moves the remaining ones
(check-expect (next-missiles (list M2 M1 M4 M3) (list I2 I1))
              (list (move-single-missile M2) (move-single-missile M1)))

;; Test the function which produces the next state of the tank
(check-expect (next-tank T0) T0)
(check-expect (next-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (next-tank T3) (make-tank (tank-x T3) -1))
(check-expect (next-tank T4) (make-tank (tank-x T4) 1))

;; Test the function which renders the tank
(check-expect (render-tank T0 BACKGROUND)
              (place-image TANK (tank-x T0) TANK-Y BACKGROUND))

;; Test the function which renders the missiles
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1 M2) BACKGROUND)
              (place-image
                MISSILE
                (missile-x M1) (missile-y M1)
                (place-image
                  MISSILE
                  (missile-x M2) (missile-y M2)
                  BACKGROUND)))

;; Test the function which renders the invaders
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I0 I1) BACKGROUND)
              (place-image
                INVADER
                (invader-x I0) (invader-y I0)
                (place-image
                  INVADER
                  (invader-x I1) (invader-y I1)
                  BACKGROUND)))

;; Test the function which renders the entire game state. As we have already tested the
;; functions rendering each individual parts, we can use those functions here.

(check-expect (render-state GS0)
              (render-invaders
                (game-state-invaders GS0)
                (render-missiles
                  (game-state-missiles GS0)
                  (render-tank
                    (game-state-tank GS0) BACKGROUND))))

(check-expect (render-state GS2)
              (render-invaders
                (game-state-invaders GS2)
                (render-missiles
                  (game-state-missiles GS2)
                  (render-tank
                    (game-state-tank GS2) BACKGROUND))))

;; Test the function which handles the space bar key pressed.
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


;; Test the function which handles the left arrow key pressed.
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

;; Test the function which handles the right arrow key pressed.
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

;; Test the function which will dispatch the game state to the appropriate key
;; handle functions. We have already checked the individual key handling functions
;; and so we will use that to check whether the appropriate function is being called
;; as per the key event.

(check-expect (handle-key GS0 " ") (handle-key-space GS0))
(check-expect (handle-key GS0 "r") GS0)
(check-expect (handle-key GS1 "left") (handle-key-left GS1))
(check-expect (handle-key GS3 "right") (handle-key-right GS3))

;; Test the function which determines whether the invader made a touchdown on our base
(check-expect (invader-touchdown? I1) false)
(check-expect (invader-touchdown? I2) true)
(check-expect (invader-touchdown? I3) true)

;; Test the function which determines whether game is over or not
(check-expect (game-over? GS0) false)
(check-expect (game-over? GS2) false)
(check-expect (game-over? GS3) true)
(check-expect (game-over? GS4) true)

;; Test the function which renders the final image when the game is over
(check-expect (last-image GS4)
              (place-image
                (text "GAME OVER!" 24 "black")
                (/ WIDTH 2) (/ HEIGHT 2)
                (render-state GS4)))

;; Uncomment the below line to start the game!
(main GS0)
