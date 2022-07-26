;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 528-bezier-curve) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TOO-SMALL 10)

; Posn Posn -> Number
; calculate the distance between two points
(check-expect (distance (make-posn 0 0) (make-posn 10 0)) 10)
(check-expect (distance (make-posn 0 0) (make-posn 0 20)) 20)

(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; Posn Posn Posn -> Boolean
; returns #true if the given bezier curve is too small to subdivide
(check-expect (too-small? (make-posn 0 0) (make-posn 10 10) (make-posn TOO-SMALL 0)) #true)
(check-expect (too-small? (make-posn 0 0) (make-posn 10 10) (make-posn (add1 TOO-SMALL) 0)) #false)

(define (too-small? A B C)
  (<= (distance A C) TOO-SMALL))

; Posn Posn -> Posn
; return a mid-point between A and B
(check-expect (mid-point (make-posn 0 0) (make-posn 10 0)) (make-posn 5 0))
(check-expect (mid-point (make-posn 0 0) (make-posn 0 10)) (make-posn 0 5))

(define (mid-point A B)
  (make-posn (* 1/2 (+ (posn-x A) (posn-x B)))
             (* 1/2 (+ (posn-y A) (posn-y B)))))

; Image Posn Posn -> Image
; draws a line from A to C onto the scene
(define (draw-line scene A C)
  (scene+line scene (posn-x A) (posn-y A) (posn-x C) (posn-y C) "red"))

; Image Posn Posn Posn -> Image
; draws a bezier curve connecting A and C relative to perspective point B
(define (bezier-curve scene0 A B C)
  (cond
    [(too-small? A B C) (draw-line scene0 A C)]
    [else (local ((define A-B (mid-point A B))
                  (define B-C (mid-point B C))
                  (define A-B-C (mid-point A-B B-C))
                  (define scene1 (bezier-curve scene0 A A-B A-B-C))
                  (define scene2 (bezier-curve scene1 A-B-C B-C C)))
            scene2)]))

(bezier-curve (empty-scene 200 200) (make-posn 20 10) (make-posn 40 180) (make-posn 160 100))
