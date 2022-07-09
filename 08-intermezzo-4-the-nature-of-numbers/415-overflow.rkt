;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 415-overflow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> N
; determines the integer n such that (expt #i10.0 n) is an inexact number
; while (expt #i10.0 (+ n 1)) is approximated with +inf.0
(check-expect (overflow 0) 308)

(define (overflow n)
  (if (equal? (expt #i10.0 n) +inf.0)
      (sub1 n)
      (overflow (add1 n))))
