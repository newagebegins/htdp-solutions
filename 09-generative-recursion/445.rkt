;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |445|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.00001)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number -> Boolean
(define (poly-root? x)
  (<= (abs (poly x)) ε))

(check-satisfied 4 poly-root?)
(check-satisfied 2 poly-root?)
