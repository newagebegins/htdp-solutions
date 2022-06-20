;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |043|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A AnimationState is a Number.
; interpretation: the number of clock ticks since the animation started

(define WHEEL-RADIUS 5)
(define (u x)
  (* x WHEEL-RADIUS))

(define WORLD-WIDTH (u 50))
(define WORLD-HEIGHT (u 9))
(define CAR-WIDTH (u 10))
(define CAR-HALF-WIDTH (/ CAR-WIDTH 2))
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define WHEELS
  (overlay/offset
   (circle WHEEL-RADIUS "solid" "black")
   (u 6) 0
   (circle WHEEL-RADIUS "solid" "black")))

(define CAR
  (overlay/offset
   WHEELS
   0 (u -2)
   (above
    (rectangle (u 4) (u 2) "solid" "red")
    (rectangle CAR-WIDTH (u 2) "solid" "red"))))

(define CAR-SPEED 3)

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define X-TREE 100)
(define Y-TREE 28)

; AnimationState -> Number
; get x position of the right-most edge of the car
(define (x-car t)
  (- (* t CAR-SPEED) CAR-HALF-WIDTH))

; AnimationState -> Image
; displays the car at time t
(define (render t)
  (place-image
   CAR
   (x-car t)
   (+ (* (sin (* t 0.1)) (/ WORLD-HEIGHT 2) 0.5) 20)
   (place-image tree X-TREE Y-TREE BACKGROUND)))

; AnimationState -> Boolean
; returns #true when the car is out of sight
(define (end? t)
  (> (- (x-car t) CAR-WIDTH) WORLD-WIDTH))

; AnimationState -> AnimationState
; launches the program from some initial state
; run with (main 0)
(define (main ws)
  (big-bang ws
    [on-tick add1]
    [to-draw render]
    [stop-when end?]))
