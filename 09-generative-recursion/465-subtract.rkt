;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 465-subtract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An Equation is a [List-of Number].
; constraint: an Equation contains at least two numbers.
; interpretation: if (list a1 ... an b) is an Equation,
; a1, ..., an are the left-hand-side variable coefficients
; and b is the right-hand side

; Equation Equation -> Equation
; subtracts a multiple of e2 from e1, item by item, so that
; the resulting equation has a 0 in the first position
; assume: e1 and e2 are of equal length
(check-expect (subtract '(2 5 12 31) '(2 2 3 10)) '(3 9 21))
(check-expect (subtract '(4 1 -2 1) '(2 2 3 10)) '(-3 -8 -19))
(check-expect (subtract '(2 5 9) '(4 8 10)) '(1 4))

(define (subtract e1 e2)
  (local ((define c (/ (first e1) (first e2))))
    (for/list ([i (rest e1)] [j (rest e2)])
      (- i (* c j)))))
