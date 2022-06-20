;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |224|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TANK-SPEED 2)

; A UFO is a Posn.
; interp. (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interp. (make-tank x dx) specifies the position:
; (x, TANK-Y) and the tank's speed: dx pixels/tick

; A Missile is Posn

; A List-of-missiles is one of:
; - '()
; - (cons Missile List-of-missiles)

(define-struct sigs [ufo tank missiles])
; A SIGS is a structure:
;   (make-sigs UFO Tank List-of-posns)
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
; render a single missile onto the given image
(check-expect (render-missile (make-posn 30 40) BACKGROUND)
              (place-image MISSILE 30 40 BACKGROUND))

(define (render-missile m img)
  (place-image MISSILE (posn-x m) (posn-y m) img))

; List-of-missiles Image -> Image
; render the fired missiles onto the given image
(check-expect (render-missiles '() BACKGROUND)
              BACKGROUND)
(check-expect (render-missiles (list (make-posn 100 50)) BACKGROUND)
              (place-image MISSILE 100 50 BACKGROUND))
(check-expect (render-missiles (list (make-posn 100 50) (make-posn 50 40)) BACKGROUND)
              (place-image MISSILE 100 50
                           (place-image MISSILE 50 40 BACKGROUND)))

(define (render-missiles l img)
  (cond
    [(empty? l) img]
    [else (render-missile (first l) (render-missiles (rest l) img))]))

; SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to
; the BACKGROUND scene

; missile is not yet fired
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) '()))
              (place-image UFO 10 20
                           (place-image TANK 28 TANK-Y BACKGROUND)))

; missile almost hit the UFO
(check-expect (si-render (make-sigs (make-posn 20 100) (make-tank 100 3) (list (make-posn 22 107))))
              (place-image UFO 20 100
                           (place-image TANK 100 TANK-Y
                                        (place-image MISSILE 22 107 BACKGROUND))))

; just fired
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) (list (make-posn 28 MISSILE-INIT-Y))))
              (place-image UFO 10 20
                           (place-image TANK 28 TANK-Y
                                        (place-image MISSILE 28 MISSILE-INIT-Y BACKGROUND))))

(define (si-render s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (render-missiles (sigs-missiles s) BACKGROUND))))

; UFO -> Boolean
; produce true when the UFO has landed
(check-expect (ufo-landed? (make-posn 100 UFO-LANDED-Y)) #true)
(check-expect (ufo-landed? (make-posn 59 (- UFO-LANDED-Y 1))) #false)
(check-expect (ufo-landed? (make-posn 70 (+ UFO-LANDED-Y 1))) #true)

(define (ufo-landed? u)
  (>= (posn-y u) UFO-LANDED-Y))

; UFO Missile -> Boolean
; produce true when a missile has collided with the UFO
(define (ufo-hit1? u m)
  (<= (sqrt (+ (sqr (- (posn-x m) (posn-x u)))
               (sqr (- (posn-y m) (posn-y u)))))
      HIT-RADIUS))

; UFO List-of-missiles -> Boolean
; produce true when one of the missiles in the list has collided with the UFO
(check-expect (ufo-hit? (make-posn 100 100) '()) #false)
(check-expect (ufo-hit? (make-posn 100 100) (list (make-posn (+ 100 HIT-RADIUS) 100))) #true)
(check-expect (ufo-hit? (make-posn 70 80) (list (make-posn 70 (+ 80 HIT-RADIUS)))) #true)
(check-expect (ufo-hit? (make-posn 70 80) (list (make-posn 70 (- 80 HIT-RADIUS)))) #true)
(check-expect (ufo-hit? (make-posn 100 100) (list (make-posn 103 97))) #true)
(check-expect (ufo-hit? (make-posn 100 100) (list (make-posn (+ 100 HIT-RADIUS 1) 100))) #false)
(check-expect (ufo-hit? (make-posn 55 120) (list (make-posn (+ 55 HIT-RADIUS) (+ 120 HIT-RADIUS 1)))) #false)

(define (ufo-hit? u l)
  (cond
    [(empty? l) #false]
    [else (or (ufo-hit1? u (first l)) (ufo-hit? u (rest l)))]))

; SIGS -> Boolean
; produce true when the UFO has landed or missile hit the UFO

; the UFO has landed and the missile has not been fired yet
(check-expect
 (si-game-over?
  (make-sigs (make-posn 100 UFO-LANDED-Y)
             (make-tank 50 -3)
             '()))
 #true)

; the UFO has landed and the missile has been fired
(check-expect
 (si-game-over?
  (make-sigs (make-posn 100 UFO-LANDED-Y)
             (make-tank 50 -3)
             (list (make-posn 50 50))))
 #true)

; the UFO has been hit with the missile
(check-expect
 (si-game-over?
  (make-sigs (make-posn 100 100)
             (make-tank 90 -3)
             (list (make-posn 102 102))))
 #true)

; missile not fired, ufo not landed
(check-expect
 (si-game-over?
  (make-sigs (make-posn 100 50)
             (make-tank 50 -3)
             '()))
 #false)

; missile fired, ufo not landed or hit
(check-expect
 (si-game-over?
  (make-sigs (make-posn 100 100)
             (make-tank 90 -3)
             (list (make-posn 120 150))))
 #false)

(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))
      (ufo-hit? (sigs-ufo s) (sigs-missiles s))))

; SIGS -> Image
; render game over screen
(check-expect
 (si-render-final (make-sigs (make-posn 100 UFO-LANDED-Y) (make-tank 50 -3) '()))
 (overlay GAME-OVER (si-render (make-sigs (make-posn 100 UFO-LANDED-Y) (make-tank 50 -3) '()))))

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
; move a missile up
(check-expect (move-missile (make-posn 70 99)) (make-posn 70 (- 99 MISSILE-SPEED)))

(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

; List-of-missiles -> List-of-missiles
; move missiles up
(check-expect (move-missiles '()) '())
(check-expect (move-missiles (list (make-posn 70 99))) (list (make-posn 70 (- 99 MISSILE-SPEED))))

(define (move-missiles l)
  (cond
    [(empty? l) '()]
    [else (cons (move-missile (first l)) (move-missiles (rest l)))]))

; SIGS -> SIGS
; move UFO, tank and (possibly) missile each tick

; tank is moving right
(check-random (si-move (make-sigs (make-posn 100 20)
                                  (make-tank 70 3)
                                  '()))
              (make-sigs (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                         (make-tank 73 3)
                         '()))

; tank is moving left
(check-random (si-move (make-sigs (make-posn 40 33)
                                  (make-tank 50 -3)
                                  '()))
              (make-sigs (make-posn (ufo-new-x 40) (+ 33 UFO-SPEED))
                         (make-tank 47 -3)
                         '()))

; tank has reached the right edge of the scene
(check-random (si-move (make-sigs (make-posn 100 20)
                                  (make-tank (- TANK-MAX-X 1) 3)
                                  '()))
              (make-sigs (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                         (make-tank TANK-MAX-X 3)
                         '()))

; tank has reached the left edge of the scene
(check-random (si-move (make-sigs (make-posn 100 20)
                                  (make-tank (+ TANK-MIN-X 1) -3)
                                  '()))
              (make-sigs (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                         (make-tank TANK-MIN-X -3)
                         '()))

; missile is moving up
(check-random (si-move (make-sigs (make-posn 100 20)
                                  (make-tank 70 3)
                                  (list (make-posn 80 120))))
              (make-sigs (make-posn (ufo-new-x 100) (+ 20 UFO-SPEED))
                         (make-tank 73 3)
                         (list (make-posn 80 (- 120 MISSILE-SPEED)))))

(define (si-move s)
  (make-sigs (move-ufo (sigs-ufo s))
             (move-tank (sigs-tank s))
             (move-missiles (sigs-missiles s))))

; Tank -> Tank
; make tank move left
(check-expect (steer-tank-left (make-tank 100 TANK-SPEED)) (make-tank 100 (- TANK-SPEED)))
(check-expect (steer-tank-left (make-tank 70 (- TANK-SPEED))) (make-tank 70 (- TANK-SPEED)))

(define (steer-tank-left t)
  (make-tank (tank-loc t) (- TANK-SPEED)))

; Tank -> Tank
; make tank move right
(check-expect (steer-tank-right (make-tank 100 (- TANK-SPEED))) (make-tank 100 TANK-SPEED))
(check-expect (steer-tank-right (make-tank 70 TANK-SPEED)) (make-tank 70 TANK-SPEED))

(define (steer-tank-right t)
  (make-tank (tank-loc t) TANK-SPEED))

; Tank -> Posn
; create a missile near the tank
(check-expect (fire-missile (make-tank 60 TANK-SPEED)) (make-posn 60 MISSILE-INIT-Y))

(define (fire-missile t)
  (make-posn (tank-loc t) MISSILE-INIT-Y))

; SIGS KeyEvent -> SIGS
; handle key presses

; change tank direction to left
(check-expect (si-control (make-sigs (make-posn 100 20) (make-tank 100 TANK-SPEED) '())
                          "left")
              (make-sigs (make-posn 100 20) (make-tank 100 (- TANK-SPEED)) '()))
(check-expect (si-control (make-sigs (make-posn 100 20) (make-tank 100 TANK-SPEED) (list (make-posn 88 150)))
                          "left")
              (make-sigs (make-posn 100 20) (make-tank 100 (- TANK-SPEED)) (list (make-posn 88 150))))

; change tank direction to right
(check-expect (si-control (make-sigs (make-posn 100 20) (make-tank 100 (- TANK-SPEED)) '())
                          "right")
              (make-sigs (make-posn 100 20) (make-tank 100 TANK-SPEED) '()))
(check-expect (si-control (make-sigs (make-posn 100 20) (make-tank 100 (- TANK-SPEED)) (list (make-posn 88 150)))
                          "right")
              (make-sigs (make-posn 100 20) (make-tank 100 TANK-SPEED) (list (make-posn 88 150))))

; fire the missile
(check-expect (si-control (make-sigs (make-posn 70 20) (make-tank 88 TANK-SPEED) '())
                          " ")
              (make-sigs (make-posn 70 20) (make-tank 88 TANK-SPEED) (list (make-posn 88 MISSILE-INIT-Y))))

; can fire another missile
(check-expect (si-control (make-sigs (make-posn 70 20) (make-tank 88 TANK-SPEED) (list (make-posn 90 150)))
                          " ")
              (make-sigs (make-posn 70 20) (make-tank 88 TANK-SPEED) (list (make-posn 88 MISSILE-INIT-Y) (make-posn 90 150))))

; ignore other keys
(check-expect (si-control (make-sigs (make-posn 100 20) (make-tank 70 TANK-SPEED) '())
                          "up")
              (make-sigs (make-posn 100 20) (make-tank 70 TANK-SPEED) '()))

(define (si-control s ke)
  (cond
    [(key=? ke "left") (make-sigs (sigs-ufo s) (steer-tank-left (sigs-tank s)) (sigs-missiles s))]
    [(key=? ke "right") (make-sigs (sigs-ufo s) (steer-tank-right (sigs-tank s)) (sigs-missiles s))]
    [(key=? ke " ") (make-sigs (sigs-ufo s) (sigs-tank s) (cons (fire-missile (sigs-tank s)) (sigs-missiles s)))]
    [else s]))

; SIGS -> SIGS
; run the program with (main (make-sigs (make-posn 100 0) (make-tank 100 3) '()))
(define (main s)
  (big-bang s
    [to-draw si-render]
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]))
