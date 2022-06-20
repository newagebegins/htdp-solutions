;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |099|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 200)
(define UFO (overlay (rectangle 30 5 "solid" "blue") (circle 6 "solid" "blue")))
(define TANK (rectangle 30 10 "solid" "orange"))
(define MISSILE (triangle 8 "solid" "purple"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TANK-HALF-WIDTH (/ (image-width TANK) 2))
(define TANK-HALF-HEIGHT (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HALF-HEIGHT))
(define MISSILE-HALF-HEIGHT (/ (image-height MISSILE) 2))
(define MISSILE-INIT-Y (- TANK-Y TANK-HALF-HEIGHT MISSILE-HALF-HEIGHT))
(define UFO-HALF-HEIGHT (/ (image-height UFO) 2))
(define UFO-LANDED-Y (- HEIGHT UFO-HALF-HEIGHT))
(define HIT-RADIUS 12)
(define GAME-OVER (text "GAME OVER" 24 "black"))
(define UFO-JUMP-RADIUS 5)
(define UFO-SPEED 2)
(define MISSILE-SPEED (* UFO-SPEED 2))
(define TANK-MAX-X (- WIDTH TANK-HALF-WIDTH))
(define TANK-MIN-X TANK-HALF-WIDTH)

; A UFO is a Posn.
; interp. (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interp. (make-tank x dx) specifies the position:
; (x, TANK-Y) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interp. (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
; An Aim is a structure:
;   (make-aim UFO Tank).
; interp. game state before missile is fired

(define-struct fired [ufo tank missile])
; A Fired is a structure:
;   (make-fired UFO Tank Missile).
; interp. game state after missile is fired

; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; interp. represents the complete state of a
; space invader game

; Tank Image -> Image
; adds t to the given image im
(check-expect (tank-render (make-tank 100 3) BACKGROUND)
              (place-image TANK 100 TANK-Y BACKGROUND))
(define (tank-render t im)
  (place-image TANK (tank-loc t) TANK-Y im))

; UFO Image -> Image
; adds u to the given image im
(check-expect (ufo-render (make-posn 100 50) BACKGROUND)
              (place-image UFO 100 50 BACKGROUND))
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image
; adds m to the given image im
(check-expect (missile-render (make-posn 100 50) BACKGROUND)
              (place-image MISSILE 100 50 BACKGROUND))
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

; SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to
; the BACKGROUND scene

; missile is not yet fired
(check-expect (si-render (make-aim (make-posn 10 20) (make-tank 28 -3)))
              (place-image UFO 10 20
                           (place-image TANK 28 TANK-Y BACKGROUND)))

; missile almost hit the UFO
(check-expect (si-render (make-fired (make-posn 20 100) (make-tank 100 3) (make-posn 22 107)))
              (place-image UFO 20 100
                           (place-image TANK 100 TANK-Y
                                        (place-image MISSILE 22 107 BACKGROUND))))

; just fired
(check-expect (si-render (make-fired (make-posn 10 20) (make-tank 28 -3) (make-posn 28 MISSILE-INIT-Y)))
              (place-image UFO 10 20
                           (place-image TANK 28 TANK-Y
                                        (place-image MISSILE 28 MISSILE-INIT-Y BACKGROUND))))

(define (si-render s)
  (cond
    [(aim? s) (tank-render (aim-tank s)
                           (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s) (tank-render (fired-tank s)
                             (ufo-render (fired-ufo s)
                                         (missile-render (fired-missile s) BACKGROUND)))]))

; UFO -> Boolean
; produce true when the UFO has landed
(check-expect (ufo-landed? (make-posn 100 UFO-LANDED-Y)) #true)
(check-expect (ufo-landed? (make-posn 59 (- UFO-LANDED-Y 1))) #false)
(check-expect (ufo-landed? (make-posn 70 (+ UFO-LANDED-Y 1))) #true)

(define (ufo-landed? u)
  (>= (posn-y u) UFO-LANDED-Y))

; UFO Missile -> Boolean
; produce true when the missile has collided with the UFO
(check-expect (ufo-hit? (make-posn 100 100) (make-posn (+ 100 HIT-RADIUS) 100)) #true)
(check-expect (ufo-hit? (make-posn 70 80) (make-posn 70 (+ 80 HIT-RADIUS))) #true)
(check-expect (ufo-hit? (make-posn 70 80) (make-posn 70 (- 80 HIT-RADIUS))) #true)
(check-expect (ufo-hit? (make-posn 100 100) (make-posn 103 97)) #true)
(check-expect (ufo-hit? (make-posn 100 100) (make-posn (+ 100 HIT-RADIUS 1) 100)) #false)
(check-expect (ufo-hit? (make-posn 55 120) (make-posn (+ 55 HIT-RADIUS) (+ 120 HIT-RADIUS 1))) #false)

(define (ufo-hit? u m)
  (<= (sqrt (+ (sqr (- (posn-x m) (posn-x u)))
               (sqr (- (posn-y m) (posn-y u)))))
      HIT-RADIUS))

; SIGS -> Boolean
; produce true when the UFO has landed or missile hit the UFO

; the UFO has landed and the missile has not been fired yet
(check-expect
 (si-game-over?
  (make-aim (make-posn 100 UFO-LANDED-Y)
            (make-tank 50 -3)))
 #true)

; the UFO has landed and the missile has been fired
(check-expect
 (si-game-over?
  (make-fired (make-posn 100 UFO-LANDED-Y)
              (make-tank 50 -3)
              (make-posn 50 50)))
 #true)

; the UFO has been hit with the missile
(check-expect
 (si-game-over?
  (make-fired (make-posn 100 100)
              (make-tank 90 -3)
              (make-posn 102 102)))
 #true)

; missile not fired, ufo not landed
(check-expect
 (si-game-over?
  (make-aim (make-posn 100 50)
            (make-tank 50 -3)))
 #false)

; missile fired, ufo not landed or hit
(check-expect
 (si-game-over?
  (make-fired (make-posn 100 100)
              (make-tank 90 -3)
              (make-posn 120 150)))
 #false)

(define (si-game-over? s)
  (cond
    [(aim? s) (ufo-landed? (aim-ufo s))]
    [(fired? s) (or (ufo-landed? (fired-ufo s))
                    (ufo-hit? (fired-ufo s) (fired-missile s)))]))

; SIGS -> Image
; render game over screen
(check-expect
 (si-render-final (make-aim (make-posn 100 UFO-LANDED-Y) (make-tank 50 -3)))
 (overlay GAME-OVER (si-render (make-aim (make-posn 100 UFO-LANDED-Y) (make-tank 50 -3)))))

(define (si-render-final s)
  (overlay GAME-OVER (si-render s)))

; Number -> Number
; given current x-coord produce new x-coord for UFO by randomly jumping left or right inside UFO-JUMP-RADIUS
(check-random (ufo-new-x 100) (+ 100 (- (random (+ (* 2 UFO-JUMP-RADIUS) 1)) UFO-JUMP-RADIUS)))
(define (ufo-new-x x)
  (+ x (- (random (+ (* 2 UFO-JUMP-RADIUS) 1)) UFO-JUMP-RADIUS)))

; UFO -> UFO
; move UFO down and randomly jump left and right
(check-random (move-ufo (make-posn 100 20)) (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED)))
               
(define (move-ufo u)
  (make-posn (restrict-ufo-x (ufo-new-x (posn-x u))) (+ (posn-y u) UFO-SPEED)))

; Number -> Number
; keep UFO from going outside the scene horizontally
(check-expect (restrict-ufo-x (+ WIDTH 1)) WIDTH)
(check-expect (restrict-ufo-x -1) 0)
(check-expect (restrict-ufo-x (/ WIDTH 2)) (/ WIDTH 2))

(define (restrict-ufo-x x)
  (cond
    [(> x WIDTH) WIDTH]
    [(< x 0) 0]
    [else x]))

; Tank -> Tank
; move tank horizontally, tank stops moving when edge of the scene is reached
(check-expect (move-tank (make-tank 57 3)) (make-tank 60 3))
(check-expect (move-tank (make-tank 57 -3)) (make-tank 54 -3))
(check-expect (move-tank (make-tank (- TANK-MAX-X 1) 3)) (make-tank TANK-MAX-X 3))
(check-expect (move-tank (make-tank (+ TANK-MIN-X 1) -3)) (make-tank TANK-MIN-X -3))

(define (move-tank t)
  (make-tank
   (cond
     [(> (+ (tank-loc t) (tank-vel t)) TANK-MAX-X) TANK-MAX-X]
     [(< (+ (tank-loc t) (tank-vel t)) TANK-MIN-X) TANK-MIN-X]
     [else (+ (tank-loc t) (tank-vel t))])
   (tank-vel t)))

; Missile -> Missile
; move missile up
(check-expect (move-missile (make-posn 70 99)) (make-posn 70 (- 99 MISSILE-SPEED)))

(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

; SIGS -> SIGS
; move UFO, tank and (possibly) missile each tick

; tank is moving right
(check-random (si-move (make-aim (make-posn 100 20)
                                 (make-tank 70 3)))
              (make-aim (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                        (make-tank 73 3)))

; tank is moving left
(check-random (si-move (make-aim (make-posn 40 33)
                                 (make-tank 50 -3)))
              (make-aim (make-posn (ufo-new-x 40) (+ 33 UFO-SPEED))
                        (make-tank 47 -3)))

; tank has reached the right edge of the scene
(check-random (si-move (make-aim (make-posn 100 20)
                                 (make-tank (- TANK-MAX-X 1) 3)))
              (make-aim (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                        (make-tank TANK-MAX-X 3)))

; tank has reached the left edge of the scene
(check-random (si-move (make-aim (make-posn 100 20)
                                 (make-tank (+ TANK-MIN-X 1) -3)))
              (make-aim (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                        (make-tank TANK-MIN-X -3)))

; missile is moving up
(check-random (si-move (make-fired (make-posn 100 20)
                                   (make-tank 70 3)
                                   (make-posn 80 120)))
              (make-fired (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                          (make-tank 73 3)
                          (make-posn 80 (- 120 MISSILE-SPEED))))

(define (si-move s)
  (cond
    [(aim? s) (make-aim (move-ufo (aim-ufo s))
                        (move-tank (aim-tank s)))]
    [(fired? s) (make-fired (move-ufo (fired-ufo s))
                            (move-tank (fired-tank s))
                            (move-missile (fired-missile s)))]))

; SIGS -> SIGS
; run the program with (main (make-aim (make-posn 100 0) (make-tank 100 3)))
(define (main s)
  (big-bang s
    [to-draw si-render]
    [on-tick si-move]
    [stop-when si-game-over? si-render-final]))
