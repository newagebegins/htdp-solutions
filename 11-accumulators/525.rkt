;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |525|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TOO-SMALL 10)

; Image Posn Posn Posn -> Image
; adds the black triangle a, b, c to scene
(check-expect (add-triangle (empty-scene 100 100) (make-posn 10 10) (make-posn 50 10) (make-posn 30 50))
              (scene+line
               (scene+line
                (scene+line
                 (empty-scene 100 100) 30 50 10 10 "black")
                50 10 30 50 "black")
               10 10 50 10 "black"))

(define (add-triangle scene a b c)
  (scene+line
   (scene+line
    (scene+line scene (posn-x a) (posn-y a) (posn-x b) (posn-y b) "black")
    (posn-x b) (posn-y b) (posn-x c) (posn-y c) "black")
   (posn-x c) (posn-y c) (posn-x a) (posn-y a) "black"))

; Posn Posn -> Number
; calculate the distance between two points
(check-expect (distance (make-posn 0 0) (make-posn 10 0)) 10)
(check-expect (distance (make-posn 0 0) (make-posn 0 20)) 20)

(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; Posn Posn Posn -> Boolean
; is the triangle a, b, c too small to be divided
(check-expect (too-small? (make-posn 0 0)
                          (make-posn 0 TOO-SMALL)
                          (make-posn TOO-SMALL TOO-SMALL))
              #true)
(check-expect (too-small? (make-posn 0 0)
                          (make-posn 0 (add1 TOO-SMALL))
                          (make-posn TOO-SMALL TOO-SMALL))
              #false)

(define (too-small? a b c)
  (or (<= (distance a b) TOO-SMALL)
      (<= (distance b c) TOO-SMALL)
      (<= (distance c a) TOO-SMALL)))

; Posn Posn -> Posn
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 0 0) (make-posn 10 0)) (make-posn 5 0))
(check-expect (mid-point (make-posn 0 0) (make-posn 0 10)) (make-posn 0 5))

(define (mid-point a b)
  (make-posn (* 1/2 (+ (posn-x a) (posn-x b)))
             (* 1/2 (+ (posn-y a) (posn-y b)))))
