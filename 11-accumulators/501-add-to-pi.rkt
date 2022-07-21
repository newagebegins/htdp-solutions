;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 501-add-to-pi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Number
; adds n0 to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)

(define (add-to-pi n0)
  (local (; N Number -> Number
          ; accumulator: a = pi + (n0 - n)
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n0 pi)))
