;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 357-eval-definition1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name expr])

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define UNDEF-VAR "undefined variable")
(define UNDEF-FUN "undefined function")

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; replace all occurrences of x by v in ex
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (subst (fun-name ex) x v) (subst (fun-expr ex) x v))]))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; evaluate the expression ex which may contain applications of a function f
; with parameter x and body b
(check-expect (eval-definition1 123 'foo 'p 0) 123)
(check-error (eval-definition1 'x 'foo 'p 0) UNDEF-VAR)
(check-expect (eval-definition1 (make-add 1 2) 'a 'b 0) 3)
(check-expect (eval-definition1 (make-mul 3 4) 'a 'b 0) 12)
(check-expect (eval-definition1 (make-fun 'my-sqr 3) 'my-sqr 'arg (make-mul 'arg 'arg)) 9)
(check-expect (eval-definition1 (make-fun 'my-sqr (make-add 1 2)) 'my-sqr 'arg (make-mul 'arg 'arg)) 9)
(check-expect (eval-definition1 (make-fun 'my-sqr (make-fun 'my-sqr 2)) 'my-sqr 'x (make-mul 'x 'x)) 16)
(check-expect (eval-definition1 (make-fun 'foo 10) 'foo 'x 99) 99)
(check-error (eval-definition1 (make-fun 'foo 10) 'bar 'x 99) UNDEF-FUN)
(check-error (eval-definition1 (make-fun 'foo 3) 'foo 'x (make-mul 'x 'y)) UNDEF-VAR)
(check-expect (eval-definition1 (make-add (make-fun 'my-add1 5) 4) 'my-add1 'x (make-add 'x 1)) 10)
(check-expect (eval-definition1 (make-mul (make-fun 'my-add1 5) 4) 'my-add1 'x (make-add 'x 1)) 24)
(check-expect (eval-definition1 (make-fun 'foo 3) 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x)))
              (* (+ 3 1) (* 3 3)))
(check-expect (eval-definition1 (make-add (make-mul 2 3) (make-mul 1 2)) 'x 'y 0) 8)
(check-expect (eval-definition1 (make-mul (make-mul 2 3) (make-mul 1 2)) 'x 'y 0) 12)
(check-error (eval-definition1 (make-fun 'f 1) 'f 'x (make-fun 'g 'x)) UNDEF-FUN)

(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error UNDEF-VAR)]
    [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                  (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                  (eval-definition1 (mul-right ex) f x b))]
    [(fun? ex) (if (equal? (fun-name ex) f)
                   (local ((define value (eval-definition1 (fun-expr ex) f x b))
                           (define plugd (subst b x value)))
                     (eval-definition1 plugd f x b))
                   (error UNDEF-FUN))]))

; infinite recursion:
; (eval-definition1 (make-fun 'f 1) 'f 'x (make-fun 'f 'x))
