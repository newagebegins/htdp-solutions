;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |273|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X -> Y] [List-of X] -> [List-of Y]
(check-expect (my-map posn-x (list (make-posn 1 2) (make-posn 3 4))) '(1 3))
(check-expect (my-map add1 '(1 2 3)) '(2 3 4))

(define (my-map f l)
  (local (; X [List-of Y] -> [List-of Y]
          (define (g x ly)
            (cons (f x) ly)))
    (foldr g '() l)))
