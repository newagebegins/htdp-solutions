;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |045|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; It represents distance of the center of the cat image from the left edge of the scene.

(define CAT (bitmap "../assets/cat.png"))
(define CAT-WIDTH (image-width CAT))
(define CAT-HEIGHT (image-height CAT))
(define CAT-HALF-WIDTH (/ CAT-WIDTH 2))
(define CAT-HALF-HEIGHT (/ CAT-HEIGHT 2))
(define CAT-Y CAT-HALF-HEIGHT)

(define MOVEMENT-PER-TICK 3) ; a number of pixels the cat moves per clock tick

(define SCENE-WIDTH (* CAT-WIDTH 2))
(define SCENE-HEIGHT CAT-HEIGHT)
(define SCENE (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "white"))

; WorldState -> WorldState
; launches the program with the given starting position of the cat
; run with (cat-prog 0)
(define (cat-prog x)
  (big-bang x
    [on-tick move-cat]
    [to-draw draw-cat]))

; WorldState -> WorldState
; Move the cat a few pixels to the right.
; Whenever the cat disappears on the right, it reappears on the left.
(check-expect (move-cat 0) MOVEMENT-PER-TICK)
(check-expect (move-cat 10) (+ 10 MOVEMENT-PER-TICK))
(check-expect (move-cat SCENE-WIDTH) MOVEMENT-PER-TICK)
(define (move-cat x)
  (modulo (+ x MOVEMENT-PER-TICK) SCENE-WIDTH))

; WorldState -> Image
; draw the cat at the given position
(define (draw-cat x)
  (place-image CAT x CAT-Y SCENE))
