;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |151|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of:
; - 0
; - (add1 N)
; interp. represents the counting numbers

; N Number -> Number
; multiply n by x without using *
(check-expect (multiply 0 1.5) 0)
(check-expect (multiply 2 3) 6)
(check-expect (multiply 3 2) 6)
(check-expect (multiply 1 100) 100)
(check-expect (multiply 2 -3.3) -6.6)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ x (multiply (sub1 n) x))]))
