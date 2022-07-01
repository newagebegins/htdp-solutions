;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 355-eval-var-lookup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define UNDEFINED-VARIABLE-ERROR "undefined variable")

; BSL-var-expr AL -> Number
; evaluate an expression using a list of variable values
(check-expect (eval-var-lookup 1 '()) 1)
(check-expect (eval-var-lookup 'a '((a 8))) 8)
(check-expect (eval-var-lookup 'a '((x 2) (a 3))) 3)
(check-error (eval-var-lookup 'a '()))
(check-error (eval-var-lookup 'a '((b 2))))
(check-expect (eval-var-lookup (make-add 'x 'y) '((x 3) (y 4))) 7)
(check-expect (eval-var-lookup (make-mul 'x 'y) '((x 3) (y 4))) 12)
(check-expect (eval-var-lookup (make-add (make-mul 'x 3) (make-add 4 'x)) '((x 2)))
              (+ (* 2 3) (+ 4 2)))
(check-expect (eval-var-lookup (make-mul (make-mul 'x 3) (make-add 4 'x)) '((x 2)))
              (* (* 2 3) (+ 4 2)))
(check-error (eval-var-lookup (make-mul (make-mul 'x 3) (make-add 4 'y)) '((x 2))))

(define (eval-var-lookup e da)
  (cond
    [(number? e) e]
    [(symbol? e) (local ((define v (assq e da)))
                   (if (false? v)
                       (error UNDEFINED-VARIABLE-ERROR)
                       (second v)))]
    [(add? e) (+ (eval-var-lookup (add-left e) da)
                 (eval-var-lookup (add-right e) da))]
    [(mul? e) (* (eval-var-lookup (mul-left e) da)
                 (eval-var-lookup (mul-right e) da))]))
