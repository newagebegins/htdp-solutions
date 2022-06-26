;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 320-count-simplified) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An S-expr is one of:
; – Number
; – String
; – Symbol
; – [List-of S-expr]

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'hello 'hello) 1)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(check-expect (count '(((world "foo") hello 3) hello) 'hello) 2)

(define (count sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [else (for/sum ([s sexp]) (count s sy))]))
