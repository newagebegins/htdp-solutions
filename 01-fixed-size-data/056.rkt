;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |056|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An LRCD (short for launching rocket countdown) is one of:
; - "resting"
; - a Number between -3 and -1
; - NonnegativeNumber
; interpretation: a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

(define HEIGHT 300) ; distances in pixels
(define WIDTH  100)
(define YDELTA 3)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))

; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-key launch]
    [on-tick fly]
    [stop-when out-of-sight?]))

; LRCD -> Image
; renders the state as a resting or flying rocket

(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)))

(check-expect
 (show 0)
 (place-image ROCKET 10 (- CENTER) BACKG))

(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG))

(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))

(define (show x)
  (cond
    [(string? x) (render-rocket HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (render-rocket HEIGHT))]
    [(>= x 0) (render-rocket x)]))

; NonnegativeNumber -> Image
; renders the rocket at the given height h

(check-expect (render-rocket 53) (place-image ROCKET 10 (- 53 CENTER) BACKG))

(define (render-rocket h)
  (place-image ROCKET 10 (- h CENTER) BACKG))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

; LRCD -> Boolean
; returns #true when the rocket is out of sight

(check-expect (out-of-sight? "resting") #false)
(check-expect (out-of-sight? -3) #false)
(check-expect (out-of-sight? -1) #false)
(check-expect (out-of-sight? HEIGHT) #false)
(check-expect (out-of-sight? 30) #false)
(check-expect (out-of-sight? 0) #true)

(define (out-of-sight? x)
  (cond
    [(string? x) #false]
    [(= x 0) #true]
    [else #false]))
