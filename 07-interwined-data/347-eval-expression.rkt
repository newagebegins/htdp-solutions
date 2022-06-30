;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 347-eval-expression) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; BSL-expr -> Number
; compute the value of a BSL expression
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 3 4)) (+ 3 4))
(check-expect (eval-expression (make-mul 5 6)) (* 5 6))
(check-expect (eval-expression (make-add (make-mul (make-add 2 3)
                                                   (make-mul 1 2))
                                         (make-add 5 5)))
              (+ (* (+ 2 3)
                    (* 1 2))
                 (+ 5 5)))

(define (eval-expression expr)
  (cond
    [(number? expr) expr]
    [(add? expr) (+ (eval-expression (add-left expr))
                    (eval-expression (add-right expr)))]
    [(mul? expr) (* (eval-expression (mul-left expr))
                    (eval-expression (mul-right expr)))]))
