;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |146|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NEList-of-temperatures is one of:
; - (cons CTemperature '())
; - (cons CTemperature List-of-temperatures)

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; NEList-of-temperatures -> Number
; computes the average temperature
(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (average alot)
  (/ (sum alot) (how-many alot)))

; NEList-of-temperatures -> Number
; adds up the temperatures on the given list
(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum alot)
  (cond
    [(empty? (rest alot)) (first alot)]
    [else (+ (first alot) (sum (rest alot)))]))

; NEList-of-temperatures -> Number
; counts the temperatures on the given list
(check-expect (how-many (cons 1 (cons 2 (cons 3 '())))) 3)

(define (how-many alot)
  (cond
    [(empty? (rest alot)) 1]
    [else (+ 1 (how-many (rest alot)))]))
