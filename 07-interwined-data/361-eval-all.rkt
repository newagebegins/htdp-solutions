;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 361-eval-all) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name expr])
; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

(define-struct con-def [name expr])
; BSL-con-def is a structure:
;   (make-con-def Symbol BSL-fun-expr)
; interpretation: constant definition

(define-struct fun-def [name param body])
; BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation: (make-fun-def n p b) represents a function definition with name n,
; parameter p and body b

; BSL-da-all is one of:
; - '()
; - (cons BSL-con-def BSL-da-all)
; - (cons BSL-fun-def BSL-da-all)
; interpretation: represents a definitions area with constant and function definitions

; (define close-to-pi 3.14)
(define close-to-pi-def
  (make-con-def 'close-to-pi 3.14))

; (define (area-of-circle r)
;   (* close-to-pi (* r r)))
(define area-of-circle-def
  (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))

; (define (volume-of-10-cylinder r)
;   (* 10 (area-of-circle r)))
(define volume-of-10-cylinder-def
  (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fun 'area-of-circle 'r))))

; BSL-da-all
(define DA1 (list close-to-pi-def
                  area-of-circle-def
                  volume-of-10-cylinder-def))

(define UNDEF-VAR "undefined variable")
(define UNDEF-FUN "undefined function")

; BSL-da-all Symbol -> BSL-con-def
; produces the representation of a constant definition whose name is x,
; if such a piece of data exists in da
(check-expect (lookup-con-def DA1 'close-to-pi) close-to-pi-def)
(check-error (lookup-con-def DA1 'area-of-circle) UNDEF-VAR)
(check-error (lookup-con-def '() 'close-to-pi) UNDEF-VAR)

(define (lookup-con-def da x)
  (cond
    [(empty? da) (error UNDEF-VAR)]
    [else (if (and (con-def? (first da))
                   (symbol=? (con-def-name (first da)) x))
              (first da)
              (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> BSL-fun-def
; produces the representation of a function definition whose name is f,
; if such a piece of data exists in da
(check-expect (lookup-fun-def DA1 'area-of-circle) area-of-circle-def)
(check-expect (lookup-fun-def DA1 'volume-of-10-cylinder) volume-of-10-cylinder-def)
(check-error (lookup-fun-def DA1 'close-to-pi) UNDEF-FUN)
(check-error (lookup-fun-def '() 'area-of-circle) UNDEF-FUN)

(define (lookup-fun-def da f)
  (cond
    [(empty? da) (error UNDEF-FUN)]
    [else (if (and (fun-def? (first da))
                   (symbol=? (fun-def-name (first da)) f))
              (first da)
              (lookup-fun-def (rest da) f))]))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; replace all occurrences of x by v in ex
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (subst (fun-name ex) x v) (subst (fun-expr ex) x v))]))

; BSL-fun-expr BSL-da-all -> Number
; evaluate an expression with the given definitions area
(check-expect (eval-all 123 '()) 123)
(check-error (eval-all 'x '()) UNDEF-VAR)
(check-expect (eval-all (make-add 1 2) '()) 3)
(check-expect (eval-all (make-mul 3 4) '()) 12)
(check-expect (eval-all (make-fun 'my-sqr 3)
                        (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-all (make-fun 'my-sqr (make-add 1 2))
                        (list (make-fun-def 'my-sqr 'arg (make-mul 'arg 'arg))))
              9)
(check-expect (eval-all (make-fun 'my-sqr (make-fun 'my-sqr 2))
                        (list (make-fun-def 'my-sqr 'x (make-mul 'x 'x))))
              16)
(check-expect (eval-all (make-fun 'foo 10)
                        (list (make-fun-def 'foo 'x 99)))
              99)
(check-error (eval-all (make-fun 'foo 10)
                       (list (make-fun-def 'bar 'x 99)))
             UNDEF-FUN)
(check-error (eval-all (make-fun 'foo 3)
                       (list (make-fun-def 'foo 'x (make-mul 'x 'y))))
             UNDEF-VAR)
(check-expect (eval-all (make-add (make-fun 'my-add1 5) 4)
                        (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              10)
(check-expect (eval-all (make-mul (make-fun 'my-add1 5) 4)
                        (list (make-fun-def 'my-add1 'x (make-add 'x 1))))
              24)
(check-expect (eval-all (make-fun 'foo 3)
                        (list (make-fun-def 'foo 'x (make-mul (make-add 'x 1) (make-mul 'x 'x)))))
              (* (+ 3 1) (* 3 3)))
(check-expect (eval-all (make-add (make-mul 2 3) (make-mul 1 2)) '()) 8)
(check-expect (eval-all (make-mul (make-mul 2 3) (make-mul 1 2)) '()) 12)
(check-error (eval-all (make-fun 'f 1)
                       (list (make-fun-def 'f 'x (make-fun 'g 'x))))
             UNDEF-FUN)
(check-expect (eval-all (make-add (make-fun 'f 3) (make-fun 'g 4))
                        (list (make-fun-def 'f 'x (make-fun 'h 'x))
                              (make-fun-def 'g 'y (make-mul 'y 'y))
                              (make-fun-def 'h 'z (make-add 'z 1))))
              (+ 4 16))
(check-expect (eval-all (make-fun 'volume-of-10-cylinder 5) DA1)
              (* 10 (* 3.14 (* 5 5))))
(check-expect (eval-all (make-add 'x 1)
                        (list (make-con-def 'x (make-fun 'f 3))
                              (make-fun-def 'f 'y (make-mul 'y 'y))))
              (+ (* 3 3) 1))

(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (local ((define cdef (lookup-con-def da ex)))
                    (eval-all (con-def-expr cdef) da))]
    [(add? ex) (+ (eval-all (add-left ex) da)
                  (eval-all (add-right ex) da))]
    [(mul? ex) (* (eval-all (mul-left ex) da)
                  (eval-all (mul-right ex) da))]
    [(fun? ex) (local ((define arg-val (eval-all (fun-expr ex) da))
                       (define fdef (lookup-fun-def da (fun-name ex)))
                       (define fbody-after-subst (subst (fun-def-body fdef) (fun-def-param fdef) arg-val)))
                 (eval-all fbody-after-subst da))]))
