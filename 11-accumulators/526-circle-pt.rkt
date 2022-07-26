;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 526-circle-pt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define EPSILON 0.01)

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels

; Number -> Posn
; determines the point on the circle with CENTER
; and RADIUS whose angle is deg
(check-within (circle-pt 0) (make-posn 400 200) EPSILON)
(check-within (circle-pt 90) (make-posn 200 0) EPSILON)
(check-within (circle-pt 180) (make-posn 0 200) EPSILON)
(check-within (circle-pt 270) (make-posn 200 400) EPSILON)

(define (circle-pt deg)
  (local ((define rad (* deg (/ pi 180))))
    (make-posn (+ (posn-x CENTER) (* RADIUS (cos rad)))
               (- (posn-y CENTER) (* RADIUS (sin rad))))))

; test image
(define P1 (circle-pt 0))
(define P2 (circle-pt 120))
(define P3 (circle-pt 240))
(place-image (circle 5 "solid" "red") (posn-x P1) (posn-y P1)
             (place-image (circle 5 "solid" "green") (posn-x P2) (posn-y P2)
                          (place-image (circle 5 "solid" "blue") (posn-x P3) (posn-y P3)
                                       (circle RADIUS "outline" "black"))))
