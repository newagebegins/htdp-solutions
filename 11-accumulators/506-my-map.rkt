;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 506-my-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [List-of X] [X -> Y] -> [List-of Y]
; works like map
(check-expect (my-map '(1 2 3) add1) '(2 3 4))
(check-expect (my-map '((a b) (c d) (e f)) first) '(a c e))

(define (my-map l0 f)
  (local (; [X Y] [List-of X] [List-of Y] -> [List-of Y]
          ; accumulator: a is a reversed list of already processed items from l0
          (define (my-map/a l a)
            (cond
              [(empty? l) a]
              [else (my-map/a (rest l) (cons (f (first l)) a))])))
    (reverse (my-map/a l0 '()))))
