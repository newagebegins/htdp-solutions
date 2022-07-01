;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 354-eval-variable) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

(define UNDEFINED-VARIABLE-ERROR "undefined variable")

; BSL-var-expr -> Number
; evaluate an expression
(check-expect (eval-variable 1) 1)
(check-error (eval-variable 'a))
(check-expect (eval-variable (make-add 1 2)) 3)
(check-expect (eval-variable (make-mul 3 4)) 12)
(check-expect (eval-variable (make-add (make-mul 2 3) (make-add 3 4))) (+ (* 2 3) (+ 3 4)))
(check-expect (eval-variable (make-mul (make-add 5 5) (make-mul 9 9))) (* (+ 5 5) (* 9 9)))
(check-error (eval-variable (make-add (make-mul 2 3) (make-add 'foo 4))))

(define (eval-variable ex)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error UNDEFINED-VARIABLE-ERROR)]
    [(add? ex) (+ (eval-variable (add-left ex))
                  (eval-variable (add-right ex)))]
    [(mul? ex) (* (eval-variable (mul-left ex))
                  (eval-variable (mul-right ex)))]))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-var-expr Symbol Number -> BSL-var-expr
; replace all occurrences of x by v in ex
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]))

; BSL-var-expr AL -> Number
; evaluate an expression using a list of variable values
(check-expect (eval-variable* 1 '()) 1)
(check-expect (eval-variable* 'a '((a 8))) 8)
(check-error (eval-variable* 'a '()))
(check-error (eval-variable* 'a '((b 2))))
(check-expect (eval-variable* (make-add 'x 'y) '((x 3) (y 4))) 7)
(check-expect (eval-variable* (make-mul 'x 'y) '((x 3) (y 4))) 12)
(check-expect (eval-variable* (make-add (make-mul 'x 3) (make-add 4 'x)) '((x 2)))
              (+ (* 2 3) (+ 4 2)))
(check-expect (eval-variable* (make-mul (make-mul 'x 3) (make-add 4 'x)) '((x 2)))
              (* (* 2 3) (+ 4 2)))
(check-error (eval-variable* (make-mul (make-mul 'x 3) (make-add 4 'y)) '((x 2))))

(define (eval-variable* ex da)
  (eval-variable (foldr (lambda (as result-ex)
                          (subst result-ex (first as) (second as)))
                        ex
                        da)))
