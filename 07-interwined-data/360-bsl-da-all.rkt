;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 360-bsl-da-all) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
