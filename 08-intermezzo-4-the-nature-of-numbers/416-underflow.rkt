;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 416-underflow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Integer -> Integer
; determines the smallest integer n such that (expt #i10.0 n) is an inexact number
; while (expt #i10.0 (- n 1)) is approximated with #i0.0
(check-expect (underflow 0) -323)

(define (underflow n)
  (if (equal? (expt #i10.0 n) #i0.0)
      (add1 n)
      (underflow (sub1 n))))
