;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |251|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of the numbers on l
(check-expect (sum '(1 2 3 4)) 10)

(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of the numbers on l
(check-expect (product '(1 2 3 4)) 24)

(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [Number Number -> Number] Number [List-of Number]
(check-expect (fold1 + 0 '(1 2 3 4)) 10)
(check-expect (fold1 * 1 '(1 2 3 4)) 24)

(define (fold1 f d l)
  (cond
    [(empty? l) d]
    [else
     (f (first l)
        (fold1 f d (rest l)))]))
