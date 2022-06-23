;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |297|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number Posn -> Number
; calculate the distance between (make-posn x y) and p
(check-expect (distance-between 0 0 (make-posn 0 0)) 0)
(check-expect (distance-between 3 0 (make-posn 0 0)) 3)
(check-expect (distance-between 0 0 (make-posn 3 4)) 5)
(check-expect (distance-between 3 4 (make-posn 0 0)) 5)
(check-expect (distance-between 3 4 (make-posn 3 10)) 6)

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))
