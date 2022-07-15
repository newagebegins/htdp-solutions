;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 457-double-amount) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number -> N
; computes how many months it takes to double the given amount of money (a)
; when a savings account pays interest at a fixed rate (r) on a monthly basis
(check-expect (double-amount 10 0.01) 70)

(define (double-amount a0 r)
  (local (; Number -> N
          (define (helper a)
            (cond
              [(>= a (* 2 a0)) 0]
              [else (add1 (helper (+ a (* a r))))])))
    (helper a0)))
