;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 500-how-many) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List -> N
; determines the number of items in l0
(check-expect (how-many '()) 0)
(check-expect (how-many '(a b c d e)) 5)
(check-expect (how-many '(1 2 3)) 3)

(define (how-many l0)
  (local (; List N -> N
          ; accumulator: a counts the number of items that l lacks from l0
          (define (how-many/a l a)
            (cond
              [(empty? l) a]
              [else (how-many/a (rest l) (add1 a))])))
    (how-many/a l0 0)))
