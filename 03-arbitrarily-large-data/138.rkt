;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |138|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A PositiveNumber is a Number greater than/equal to 0.

; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)

; List-of-amounts -> PositiveNumber
; compute sum of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 2 '())) 2)
(check-expect (sum (cons 1 (cons 2 '()))) 3)
(check-expect (sum (cons 1 (cons 0 (cons 5.1 '())))) 6.1)

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa) (+ (first loa) (sum (rest loa)))]))
