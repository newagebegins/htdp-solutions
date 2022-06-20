;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |064|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; posn -> PositiveNumber
; compute Manhattan distance from p to the origin

(check-expect (manhattan-distance (make-posn 0 0)) 0)
(check-expect (manhattan-distance (make-posn 5 0)) 5)
(check-expect (manhattan-distance (make-posn -5 0)) 5)
(check-expect (manhattan-distance (make-posn 0 3)) 3)
(check-expect (manhattan-distance (make-posn 0 -3)) 3)
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn -3 -4)) 7)
(check-expect (manhattan-distance (make-posn -3 4)) 7)
(check-expect (manhattan-distance (make-posn 3 -4)) 7)

(define (manhattan-distance p)
  (+ (abs (posn-x p))
     (abs (posn-y p))))
