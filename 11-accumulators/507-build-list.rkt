;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 507-build-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] N [N -> X] -> [List-of X]
; works like build-list
(check-expect (build-l*st 0 identity) '())
(check-expect (build-l*st 1 identity) '(0))
(check-expect (build-l*st 3 identity) '(0 1 2))
(check-expect (build-l*st 4 add1) '(1 2 3 4))
(check-expect (build-l*st 2 number->string) '("0" "1"))

(define (build-l*st n0 f)
  (local (; [X] N [List-of X] -> [List-of X]
          ; accumulator: a is a result of processing numbers [n,n0)
          (define (bl/a n a)
            (cond
              [(zero? n) a]
              [else (bl/a (sub1 n) (cons (f (sub1 n)) a))])))
    (bl/a n0 '())))
