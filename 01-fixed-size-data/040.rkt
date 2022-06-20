;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |040|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation: the number of pixels between the left border of the scene and the car.

(define BACKGROUND (empty-scene 100 100))

; WorldState -> Image
; places the image of the car x pixels from the left of the BACKGROUND image
(define (render x)
  BACKGROUND)

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
; examples:
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock cw)
  (+ cw 3))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))
