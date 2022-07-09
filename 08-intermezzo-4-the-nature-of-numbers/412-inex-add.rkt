;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 412-inex-add) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Number -> S
(define (sign n)
  (if (< n 0)
      -1
      1))

; Inex Inex -> Inex
; adds two Inex representations of numbers that have the same exponent (or differ by 1)
(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0)) (create-inex 3 1 0))
(check-expect (inex+ (create-inex 1 -1 1) (create-inex 2 -1 1)) (create-inex 3 -1 1))
(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 1 1 3) (create-inex 2 1 3)) (create-inex 3 1 3))
(check-expect (inex+ (create-inex 99 1 3) (create-inex 1 1 3)) (create-inex 10 1 4))
(check-expect (inex+ (create-inex 90 1 0) (create-inex 33 1 0)) (create-inex 12 1 1))
(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1)) (create-inex 11 -1 1))
(check-expect (inex+ (create-inex 15 1 3) (create-inex 20 1 4)) (create-inex 22 1 4))
(check-expect (inex+ (create-inex 99 1 0) (create-inex 99 -1 1)) (create-inex 11 1 1))
(check-error (inex+ (create-inex 99 1 99) (create-inex 99 1 99)) "the result is out of range")
(check-error (inex+ (create-inex 1 1 0) (create-inex 1 1 2)) "exponents must not differ by more than 1")

(define (inex+ i1 i2)
  (local ((define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1))
          (define e2 (inex-exponent i2))
          (define e1s (* s1 e1))
          (define e2s (* s2 e2)))
    (if (> e2s e1s)
        (inex+ i2 i1)
        (if (> (- e1s e2s) 1)
            (error "exponents must not differ by more than 1")
            (local ((define sum (+ (* m1 (expt 10 (- e1s e2s))) m2))
                    (define m (cond
                                [(>= sum 1000) (round (/ sum 100))]
                                [(>= sum 100) (round (/ sum 10))]
                                [else sum]))
                    (define es (cond
                                 [(>= sum 1000) (+ e2s 2)]
                                 [(>= sum 100) (+ e2s 1)]
                                 [else e2s]))
                    (define e (abs es))
                    (define s (sign es)))
              (if (> e 99)
                  (error "the result is out of range")
                  (make-inex m s e)))))))
