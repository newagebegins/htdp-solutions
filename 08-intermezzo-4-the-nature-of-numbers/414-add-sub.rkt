;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 414-add-sub) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Number
; adds up n copies of #i1/185
(check-expect (add 0) 0)
(check-within (add 1) #i1/185 0.0001)
(check-within (add 185) 1 0.0001)

(define (add n)
  (cond
    [(zero? n) 0]
    [else (+ #i1/185 (add (sub1 n)))]))

; Number -> N
; counts how often 1/185 can be subtracted from x until it is 0
(check-expect (sub 0) 0)
(check-expect (sub 1/185) 1)
(check-expect (sub 1) 185)
(check-expect (sub #i1.0) 186)

(define (sub x)
  (cond
    [(<= x 0) 0]
    [else (+ 1 (sub (- x 1/185)))]))
