;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |156|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 80) ; distances in terms of pixels
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; A ShotWorld is List-of-numbers.
; interp. each number on such a list represents the y-coordinate of a shot

; ShotWorld -> Image
; adds the image of a shot for each y on w
; at (XSHOTS,y) to the background image
(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '())) (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 30 (cons 10 '())))
              (place-image SHOT XSHOTS 30
                           (place-image SHOT XSHOTS 10 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w) (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock '()) '())
(check-expect (tock (cons 50 '())) (cons 49 '()))
(check-expect (tock (cons 77 (cons 50 '()))) (cons 76 (cons 49 '())))

(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world
; if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 44 '()) " ") (cons HEIGHT (cons 44 '())))
(check-expect (keyh '() "a") '())

(define (keyh w ke)
  (if (key=? ke " ")
      (cons HEIGHT w)
      w))

; ShotWorld -> ShotWorld
; run with (main '())
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
