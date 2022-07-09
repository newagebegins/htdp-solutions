;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 413-inex-mul) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

(define OUT-OF-RANGE "the result is out of range")

; Inex Inex -> Inex
; multiply two Inex representations of numbers
(check-expect (inex* (create-inex 2 1 4) (create-inex 8 1 10))
              (create-inex 16 1 14))
(check-expect (inex* (create-inex 20 1 1) (create-inex 5 1 4))
              (create-inex 10 1 6))
(check-expect (inex* (create-inex 27 -1 1) (create-inex 7 1 4))
              (create-inex 19 1 4))
(check-expect (inex* (create-inex 99 -1 5) (create-inex 99 -1 10))
              (create-inex 98 -1 13))
(check-error (inex* (create-inex 99 1 99) (create-inex 99 1 99)) OUT-OF-RANGE)
(check-error (inex* (create-inex 99 -1 99) (create-inex 99 -1 99)) OUT-OF-RANGE)

; Number -> S
(define (sign n)
  (if (< n 0)
      -1
      1))

(define (inex* i1 i2)
  (local ((define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1))
          (define e2 (inex-exponent i2))
          (define e1s (* s1 e1))
          (define e2s (* s2 e2))
          (define m0 (* m1 m2))
          (define e0s (+ e1s e2s))
          (define m (cond
                      [(>= m0 1000) (round (/ m0 100))]
                      [(>= m0 100) (round (/ m0 10))]
                      [else m0]))
          (define e0 (cond
                       [(>= m0 1000) (+ e0s 2)]
                       [(>= m0 100) (+ e0s 1)]
                       [else e0s]))
          (define s (sign e0))
          (define e (abs e0)))
    (if (> e 99)
        (error OUT-OF-RANGE)
        (create-inex m s e))))
