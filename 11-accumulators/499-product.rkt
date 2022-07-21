;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 499-product) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the product of the numbers in l
(check-expect (product '(2 5 3 7)) 210)
(check-expect (product '(8 2 10)) 160)

(define (product l0)
  (local (; [List-of Number] Number -> Number
          ; computes the product of the numbers in l
          ; accumulator: a is the product of numbers that l lacks from l0
          (define (product/a l a)
            (cond
              [(empty? l) a]
              [else (product/a (rest l) (* (first l) a))])))
    (product/a l0 1)))
