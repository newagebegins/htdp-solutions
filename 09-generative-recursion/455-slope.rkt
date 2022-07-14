;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 455-slope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

; [Number -> Number] Number -> Number
; calculate the slope of f at r1
(check-expect (slope (lambda (x) 3) 5) 0)
(check-expect (slope (lambda (x) x) 7) 1)
(check-expect (slope (lambda (x) (* 2 x)) 3) 2)
(check-expect (slope (lambda (x) (* x x)) 3) 6)
(check-within (slope (lambda (x) (* x x x)) 2) 12 ε)

(define (slope f r1)
  (* (/ 1 (* 2 ε)) (- (f (+ r1 ε)) (f (- r1 ε)))))
