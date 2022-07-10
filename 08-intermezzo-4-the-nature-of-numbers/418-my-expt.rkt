;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 418-my-expt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number N -> Number
; raises b to the power of p
(check-expect (my-expt 2 4) (* 2 2 2 2))
(check-expect (my-expt 2 0) 1)

(define (my-expt b p)
  (cond
    [(zero? p) 1]
    [else (* b (my-expt b (sub1 p)))]))
