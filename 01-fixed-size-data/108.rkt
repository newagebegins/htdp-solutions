;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |108|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define STOP (bitmap "../assets/traffic-light-stop.png"))
(define GO (bitmap "../assets/traffic-light-go.png"))
(define MTS (empty-scene 100 100))
(define FONT-SIZE 50)

; TrafficLight is one of:
; - -1
; - Integer[10, 19]
; - Integer[0, 9]
; interp. -1 means "stop", Integer[10, 19] means "go" for 10 seconds,
;         Integer[0, 9] is a countdown when transitioning from "go" to "stop"

; TrafficLight -> Image
; render the traffic light
(check-expect (render -1) (overlay STOP MTS))
(check-expect (render 19) (overlay GO MTS))
(check-expect (render 15) (overlay GO MTS))
(check-expect (render 10) (overlay GO MTS))
(check-expect (render 9) (overlay (text "9" FONT-SIZE "orange") MTS))
(check-expect (render 8) (overlay (text "8" FONT-SIZE "green") MTS))
(check-expect (render 7) (overlay (text "7" FONT-SIZE "orange") MTS))
(check-expect (render 0) (overlay (text "0" FONT-SIZE "green") MTS))

(define (render t)
  (overlay
   (cond
     [(<= 10 t 19) GO]
     [(<= 0 t 9) (text (number->string t)
                       FONT-SIZE
                       (if (odd? t)
                           "orange"
                           "green"))]
     [else STOP])
   MTS))

; TrafficLight -> TrafficLight
; update traffic light state each tick
(check-expect (tock -1) -1)
(check-expect (tock 19) 18)
(check-expect (tock 11) 10)
(check-expect (tock 10) 9)
(check-expect (tock 1) 0)
(check-expect (tock 0) -1)

(define (tock t)
  (if (< t 0)
      t
      (sub1 t)))

; TrafficLight KeyEvent -> TrafficLight
; transition from "stop" to "go" when space bar is pressed
(check-expect (handle-keys -1 " ") 19)
(check-expect (handle-keys -1 "a") -1)
(check-expect (handle-keys 19 " ") 19)
(check-expect (handle-keys 10 " ") 10)
(check-expect (handle-keys 5 " ") 5)
(check-expect (handle-keys 0 "a") 0)

(define (handle-keys t ke)
  (if (and (< t 0) (key=? ke " "))
      19
      t))

; TrafficLight -> TrafficLight
; run with (main -1)
(define (main t)
  (big-bang t
    [to-draw render]
    [on-tick tock 1]
    [on-key handle-keys]))
