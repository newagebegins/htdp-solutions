;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 358-bsl-fun-def) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; (define (f x) (+ 3 x))
(define f (make-fun-def 'f 'x (make-add 3 'x)))

; (define (g y) (f (* 2 y)))
(define g (make-fun-def 'g 'y (make-fun 'f (make-mul 2 'y))))

; (define (h v) (+ (f v) (g v)))
(define h (make-fun-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; BSL-fun-def* is [List-of BSL-fun-def]
; interpretation: represents a definitions area that consists of a number of
; one-argument function definitions

(define da-fgh (list f g h))

(define UNDEF-FUN "undefined function")

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'f) f)
(check-expect (lookup-def da-fgh 'g) g)
(check-expect (lookup-def da-fgh 'h) h)
(check-error (lookup-def da-fgh 'a) UNDEF-FUN)
(check-error (lookup-def '() 'f) UNDEF-FUN)

(define (lookup-def da f)
  (cond
    [(empty? da) (error UNDEF-FUN)]
    [else (if (symbol=? (fun-def-name (first da)) f)
              (first da)
              (lookup-def (rest da) f))]))
