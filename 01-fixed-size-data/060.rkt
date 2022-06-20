;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |060|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An N-TrafficLight is one of:
; - 0 (the traffic light shows red)
; - 1 (the traffic light shows green)
; - 2 (the traffic light shows yellow)

; N-TrafficLight -> N-TrafficLight
; simulates a clock-based American traffic-light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next-numeric 1]))

; N-TrafficLight -> N-TrafficLight
; yields the next state, givet current state cs
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

; N-TrafficLight -> Image
; renders the current state cs as an Image
(check-expect
 (tl-render 0)
 (overlay/align
   "left" "middle"
   (beside
    (render1 "red" #true)
    (render1 "yellow" #false)
    (render1 "green" #false))
   (rectangle 49 19 "outline" "black")))

(define (tl-render cs)
  (overlay/align
   "left" "middle"
   (beside
    (render1 "red" (= cs 0))
    (render1 "yellow" (= cs 2))
    (render1 "green" (= cs 1)))
   (rectangle 49 19 "outline" "black")))

; String Boolean -> Image
; renders a single traffic light, on or off
(check-expect (render1 "red" #true) (overlay/align "right" "middle" (circle 5 "solid" "red") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "red" #false) (overlay/align "right" "middle" (circle 5 "outline" "red") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "yellow" #true) (overlay/align "right" "middle" (circle 5 "solid" "yellow") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "yellow" #false) (overlay/align "right" "middle" (circle 5 "outline" "yellow") (rectangle 15 20 0 "transparent")))
(define (render1 color on)
  (overlay/align
   "right" "middle"
   (circle 5 (if on "solid" "outline") color)
   (rectangle 15 20 0 "transparent")))
