;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |257|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] N [N -> X] -> [List-of X]
; works just like build-list
(check-expect (build-l*st 0 add1)
              (build-list 0 add1))
(check-expect (build-l*st 5 add1)
              (build-list 5 add1))
(check-expect (build-l*st 4 sub1)
              (build-list 4 sub1))
(check-expect (build-l*st 3 sqr)
              (build-list 3 sqr))

(define (build-l*st n f)
  (cond
    [(zero? n) '()]
    [else (append (build-l*st (sub1 n) f) (list (f (sub1 n))))]))
