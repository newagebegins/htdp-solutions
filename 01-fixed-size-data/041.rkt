;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |041|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation: the number of pixels between the left border of the scene and the car.

(define WHEEL-RADIUS 5)
(define (u x)
  (* x WHEEL-RADIUS))

(define WORLD-WIDTH (u 50))
(define CAR-WIDTH (u 10))
(define BACKGROUND (empty-scene WORLD-WIDTH (u 9)))

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

(define Y-CAR 32)

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define X-TREE 100)
(define Y-TREE 28)

; WorldState -> Image
; places the image of the car x pixels from the left of the BACKGROUND image
(define (render cw)
  (place-image CAR cw Y-CAR
               (place-image tree X-TREE Y-TREE BACKGROUND)))

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
; examples:
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock cw)
  (+ cw 3))

; WorldState -> Boolean
; returns #true when the car is out of sight
(define (end? cw)
  (> (- cw (/ CAR-WIDTH 2) 1) WORLD-WIDTH))

; WorldState -> WorldState
; launches the program from some initial state
; run with (main 0)
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))
