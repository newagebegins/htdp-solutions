;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |059|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; TrafficLight is one of the following Strings:
; - "red"
; - "green"
; - "yellow"
; interpretation: the three strings represent the three
; possible states that a traffic light may assume

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic-light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))

; TrafficLight -> TrafficLight
; yields the next state, givet current state cs
(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(define (tl-next cs)
  (cond
    [(string=? cs "red") "green"]
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]))

; TrafficLight -> Image
; renders the current state cs as an Image
(check-expect
 (tl-render "red")
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
    (render1 "red" (string=? cs "red"))
    (render1 "yellow" (string=? cs "yellow"))
    (render1 "green" (string=? cs "green")))
   (rectangle 49 19 "outline" "black")))

; TrafficLight Boolean -> Image
; renders a single traffic light, on or off
(check-expect (render1 "red" #true) (overlay/align "right" "middle" (circle 5 "solid" "red") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "red" #false) (overlay/align "right" "middle" (circle 5 "outline" "red") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "yellow" #true) (overlay/align "right" "middle" (circle 5 "solid" "yellow") (rectangle 15 20 0 "transparent")))
(check-expect (render1 "yellow" #false) (overlay/align "right" "middle" (circle 5 "outline" "yellow") (rectangle 15 20 0 "transparent")))
(define (render1 tl on)
  (overlay/align
   "right" "middle"
   (circle 5 (if on "solid" "outline") tl)
   (rectangle 15 20 0 "transparent")))
