;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 504-to10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Digit is 0..9

; [NEList-of Digit] -> N
; produce a decimal number from a list of digits
(check-expect (to10 '(0)) 0)
(check-expect (to10 '(7 0)) 70)
(check-expect (to10 '(1 0 2)) 102)
(check-expect (to10 '(2 3 4 5)) 2345)

(define (to10 l0)
  (local (; [List-of Digit] N N -> N
          ; accumulator: p is a power of 10 multiplier of the first digit in l
          ; accumulator: s is a decimal number with digits that l lacks from l0
          (define (to10/a l p s)
            (cond
              [(empty? l) s]
              [else (to10/a (rest l)
                            (* p 10)
                            (+ s (* (first l) p)))])))
    (to10/a (reverse l0) 1 0)))
