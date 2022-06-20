;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |047|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Happiness is a Number
; Allowed values are in the range [0,100]

; Happiness -> Happiness
; launch the program with a given happiness level
(define (gauge-prog h)
  (big-bang h
    [on-tick tock]
    [on-key handle-keys]
    [to-draw render]))

; Happiness -> Happiness
; decrease happiness by -0.1
(check-expect (tock 99.3) 99.2)
(define (tock h)
  (change h -0.1))

; Happiness -> Happiness
; Changes happiness after keypresses.
; When down arrow is pressed happiness is decreased by 1/5.
; When up arrow is pressed happiness is increased by 1/3.
(check-expect (handle-keys 10 "down") (- 10 1/5))
(check-expect (handle-keys 10 "up") (+ 10 1/3))
(check-expect (handle-keys 10 "left") 10)
(define (handle-keys h key)
  (cond
    [(string=? key "down") (change h -1/5)]
    [(string=? key "up") (change h 1/3)]
    [else h]))

; Happiness -> Image
; Renders a happiness gauge.
(check-expect (render 50)
              (overlay/align "left" "middle"
                             (rectangle 100 20 "outline" "black")
                             (rectangle 50 20 "solid" "red")))

(define (render h)
  (overlay/align "left" "middle"
   (rectangle 100 20 "outline" "black")
   (rectangle h 20 "solid" "red")))

; Happiness -> Happiness
; Change happiness h by an amount d.
; The result is always in the range [0,100].
(check-expect (change 99.3 -0.1) 99.2)
(check-expect (change 99.9 0.1) 100)
(check-expect (change 0 10) 10)
(check-expect (change 0 -0.1) 0)
(check-expect (change 100 0.1) 100)
(define (change h d)
  (cond
    [(< (+ h d) 0) 0]
    [(> (+ h d) 100) 100]
    [else (+ h d)]))
