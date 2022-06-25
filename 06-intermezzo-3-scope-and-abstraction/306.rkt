;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |306|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; N -> [List-of N]
; creates the list '(0 ... (- n 1))
(check-expect (0-to-n-1 0) '())
(check-expect (0-to-n-1 1) '(0))
(check-expect (0-to-n-1 2) '(0 1))
(check-expect (0-to-n-1 3) '(0 1 2))
(check-expect (0-to-n-1 4) '(0 1 2 3))

(define (0-to-n-1 n)
  (for/list ([i n]) i))

; N -> [List-of N]
; creates the list '(1 ... n)
(check-expect (0-to-n 0) '())
(check-expect (0-to-n 1) '(1))
(check-expect (0-to-n 2) '(1 2))
(check-expect (0-to-n 3) '(1 2 3))
(check-expect (0-to-n 4) '(1 2 3 4))

(define (0-to-n n)
  (for/list ([i n]) (add1 i)))

; N -> [List-of Number]
; creates the list '(1 1/2 ... 1/n)
(check-expect (1-to-1/n 0) '())
(check-expect (1-to-1/n 1) '(1))
(check-expect (1-to-1/n 2) '(1 1/2))
(check-expect (1-to-1/n 3) '(1 1/2 1/3))
(check-expect (1-to-1/n 4) '(1 1/2 1/3 1/4))

(define (1-to-1/n n)
  (for/list ([i n]) (/ 1 (add1 i))))

; N -> [List-of N]
; creates the list of the first n even numbers
(check-expect (evenN 0) '())
(check-expect (evenN 1) '(0))
(check-expect (evenN 2) '(0 2))
(check-expect (evenN 3) '(0 2 4))
(check-expect (evenN 4) '(0 2 4 6))
(check-expect (evenN 5) '(0 2 4 6 8))

(define (evenN n)
  (for/list ([i n]) (* 2 i)))

; N -> [List-of [List-of N]]
; creates a diagonal square of 0s and 1s
(check-expect (identityM 0) '())
(check-expect (identityM 1) '((1)))
(check-expect (identityM 2) '((1 0)
                              (0 1)))
(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))
(check-expect (identityM 4) '((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 1)))

(define (identityM n)
  (for/list ([row n])
    (for/list ([col n])
      (if (= row col) 1 0))))

; [Number -> Number] N -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list
(check-within (tabulate sin 2) (list (sin 2) (sin 1) (sin 0)) 0.001)
(check-within (tabulate sqrt 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (tabulate f n)
  (for/list ([i (add1 n)])
    (f (- n i))))
