;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |150|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of:
; - 0
; - (add1 N)
; interp. represents the counting numbers

; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 0) (+ 0 pi) 0.001)
(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; N Number -> Number
; computes (+ n pi) without using +
(check-within (add 0 pi) (+ 0 pi) 0.001)
(check-within (add 3 pi) (+ 3 pi) 0.001)
(check-within (add 5 -3.4) (+ 5 (- 3.4)) 0.001)

(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add (sub1 n) x))]))
