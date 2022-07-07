;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 398-linear-combination-value) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A LinearCombination is a list of coefficients: [List-of Number]

; LinearCombination [List-of Number] -> Number
; calculate the value of a linear combination lc given the variable values lov
; assume the lists have equal sizes
(check-expect (value (list 5) (list 10)) 50)
(check-expect (value (list 5 17) (list 10 1)) 67)
(check-expect (value (list 5 17 3) (list 10 1 2)) 73)

(define (value lc lov)
  (cond
    [(empty? lc) 0]
    [else (+ (* (first lc) (first lov))
             (value (rest lc) (rest lov)))]))
