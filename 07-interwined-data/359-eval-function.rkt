;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 359-eval-function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name expr])
; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define-struct fun-def [name param body])
; BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation: (make-fun-def n p b) represents a function definition with name n,
; parameter p and body b

; BSL-fun-def* is [List-of BSL-fun-def]
; interpretation: represents a definitions area that consists of a number of
; one-argument function definitions

(define UNDEF-VAR "undefined variable")
(define UNDEF-FUN "undefined function")

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(define (lookup-def da f)
  (cond
    [(empty? da) (error UNDEF-FUN)]
    [else (if (symbol=? (fun-def-name (first da)) f)
              (first da)
              (lookup-def (rest da) f))]))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; replace all occurrences of x by v in ex
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (subst (fun-name ex) x v) (subst (fun-expr ex) x v))]))

; BSL-fun-expr BSL-fun-def* -> Number
; evaluate an expression ex using a definitions area da
(check-expect (eval-function* 123 '()) 123)
(check-error (eval-function* 'x '()) UNDEF-VAR)
(check-expect (eval-function* (make-add 1 2) '()) 3)
(check-expect (eval-function* (make-mul 3 4) '()) 12)
(check-expect (eval-function* (make-fun 'my-sqr 3)
                              (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-function* (make-fun 'my-sqr (make-add 1 2))
                              (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-function* (make-fun 'my-sqr (make-fun 'my-sqr 2))
                              (list (make-fun-def 'my-sqr 'x (make-mul 'x 'x))))
              16)
(check-expect (eval-function* (make-fun 'foo 10)
                              (list (make-fun-def 'foo 'x 99)))
              99)
(check-error (eval-function* (make-fun 'foo 10)
                             (list (make-fun-def 'bar 'x 99)))
             UNDEF-FUN)
(check-error (eval-function* (make-fun 'foo 3)
                             (list (make-fun-def 'foo 'x (make-mul 'x 'y))))
             UNDEF-VAR)
(check-expect (eval-function* (make-add (make-fun 'my-add1 5) 4)
                              (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              10)
(check-expect (eval-function* (make-mul (make-fun 'my-add1 5) 4)
                              (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              24)
(check-expect (eval-function* (make-fun 'foo 3)
                              (list (make-fun-def 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x)))))
              (* (+ 3 1) (* 3 3)))
(check-expect (eval-function* (make-add (make-mul 2 3) (make-mul 1 2)) '()) 8)
(check-expect (eval-function* (make-mul (make-mul 2 3) (make-mul 1 2)) '()) 12)
(check-error (eval-function* (make-fun 'f 1)
                             (list (make-fun-def 'f 'x (make-fun 'g 'x))))
             UNDEF-FUN)
(check-expect (eval-function* (make-add (make-fun 'f 3) (make-fun 'g 4))
                              (list (make-fun-def 'f 'x (make-fun 'h 'x))
                                    (make-fun-def 'g 'y (make-mul 'y 'y))
                                    (make-fun-def 'h 'z (make-add 'z 1))))
              (+ 4 16))

(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error UNDEF-VAR)]
    [(add? ex) (+ (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (mul-left ex) da)
                  (eval-function* (mul-right ex) da))]
    [(fun? ex) (local ((define arg-val (eval-function* (fun-expr ex) da))
                       (define fdef (lookup-def da (fun-name ex)))
                       (define fbody-after-subst (subst (fun-def-body fdef) (fun-def-param fdef) arg-val)))
                 (eval-function* fbody-after-subst da))]))
