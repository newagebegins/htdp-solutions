;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |058|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Price falls into one of three intervals:
; - 0 through LOW (LOW not included)
; - LOW through LUXURY (LUXURY not included)
; - LUXURY and above.
; interpretation: the price of an item

(define LOW 1000)
(define LUXURY 10000)

; Price -> Number
; computes the amount of tax charged for p

(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax LOW) (* 0.05 LOW))
(check-expect (sales-tax 5789) (* 0.05 5789))
(check-expect (sales-tax LUXURY) (* 0.08 LUXURY))
(check-expect (sales-tax 12017) (* 0.08 12017))

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW)) 0]
    [(and (<= LOW p) (< p LUXURY)) (* 0.05 p)]
    [(>= p LUXURY) (* 0.08 p)]))
