;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |097|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WIDTH 200)
(define HEIGHT 200)
(define UFO (overlay (rectangle 30 5 "solid" "blue") (circle 6 "solid" "blue")))
(define TANK (rectangle 30 10 "solid" "orange"))
(define MISSILE (triangle 8 "solid" "purple"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TANK-HALF-HEIGHT (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HALF-HEIGHT))
(define MISSILE-HALF-HEIGHT (/ (image-height MISSILE) 2))
(define MISSILE-INIT-Y (- TANK-Y TANK-HALF-HEIGHT MISSILE-HALF-HEIGHT))

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
