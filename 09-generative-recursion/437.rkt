;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |437|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List -> N
; compute the length of P
(check-expect (special '(1 2 3 4)) 4)
(check-expect (special '(1 2)) 2)
(check-expect (special '()) 0)

(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))

; List -> N
(define (solve P) 0)

; List N -> N
(define (combine-solutions P n) (add1 n))

; [List-of Number] -> [List-of Number]
; negates each number in P
(check-expect (special2 '(5 -3 8 3.5 -1)) '(-5 3 -8 -3.5 1))
(check-expect (special2 '()) '())

(define (special2 P)
  (cond
    [(empty? P) (solve2 P)]
    [else
     (combine-solutions2
       P
       (special2 (rest P)))]))

; [List-of Number] -> [List-of Number]
(define (solve2 P) '())

; [List-of Number] [List-of Number] -> [List-of Number]
(define (combine-solutions2 P res) (cons (- (first P)) res))

; [List-of String] -> [List-of String]
; uppercases P
(check-expect (special3 '("hello" "world" "Foo")) '("HELLO" "WORLD" "FOO"))
(check-expect (special3 '()) '())

(define (special3 P)
  (cond
    [(empty? P) (solve3 P)]
    [else
     (combine-solutions3
       P
       (special3 (rest P)))]))

; [List-of String] -> [List-of String]
(define (solve3 P) '())

; [List-of String] [List-of String] -> [List-of String]
(define (combine-solutions3 P res) (cons (string-upcase (first P)) res))
