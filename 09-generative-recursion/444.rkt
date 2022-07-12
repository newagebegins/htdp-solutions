;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |444|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N[>= 1] N[>= 1] -> N[>= 1]
; find the greatest common divisor of S and L, S > L
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))

; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k
(check-expect (divisors 6 6) '(6 3 2 1))
(check-expect (divisors 6 25) '(5 1))
(check-expect (divisors 18 18) '(18 9 6 3 2 1))
(check-expect (divisors 18 24) '(12 8 6 4 3 2 1))

(define (divisors k l)
  (cond
    [(= k 1) '(1)]
    [else (if (zero? (remainder l k))
              (cons k (divisors (sub1 k) l))
              (divisors (sub1 k) l))]))

; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l
(check-expect (largest-common '(6 3 2 1) '(5 1)) 1)
(check-expect (largest-common '(18 9 6 3 2 1) '(12 8 6 4 3 2 1)) 6)

(define (largest-common k l)
  (if (member? (first k) l)
      (first k)
      (largest-common (rest k) l)))
