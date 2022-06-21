;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |264|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the largest number on l
(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define greatest-in-rest (sup (rest l))))
            (if (> (first l) greatest-in-rest)
                (first l)
                greatest-in-rest))]))

(sup (list 2 1 3))
