;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 395-take-and-drop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [List-of X] N -> [List-of X]
; produces the first n items from l or all of l if it is too short
(check-expect (take '() 3) '())
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(check-expect (take '(1 2 3) 2) '(1 2))
(check-expect (take '(1 2 3) 3) '(1 2 3))
(check-expect (take '(1 2 3) 4) '(1 2 3))
(check-expect (take '(a b c d) 2) '(a b))

(define (take l n)
  (cond
    [(and (cons? l) (> n 0)) (cons (first l) (take (rest l) (sub1 n)))]
    [else '()]))

; [X] [List-of X] N -> [List-of X]
; the result is l with the first n items removed or just '() if l is too short
(check-expect (drop '(a b c) 0) '(a b c))
(check-expect (drop '(a b c) 1) '(b c))
(check-expect (drop '(a b c) 2) '(c))
(check-expect (drop '(a b c) 3) '())
(check-expect (drop '(a b c) 4) '())
(check-expect (drop '() 3) '())
(check-expect (drop '(a b c d) 2) '(c d))

(define (drop l n)
  (cond
    [(empty? l) '()]
    [(= n 0) l]
    [(> n 0) (drop (rest l) (sub1 n))]))
