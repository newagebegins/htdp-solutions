;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 318-depth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of:
; – Atom
; – SL

; An Atom is one of:
; – Number
; – String
; – Symbol

; An SL is one of:
; – '()
; – (cons S-expr SL)

; Any -> Boolean
; return #true if x is an Atom
(check-expect (atom? 7) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'hello) #true)
(check-expect (atom? '()) #false)

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr -> N
; calculate the depth of an S-expression
(check-expect (depth 'hello) 1)
(check-expect (depth "hello") 1)
(check-expect (depth 1.23) 1)
(check-expect (depth '()) 1)
(check-expect (depth '(foo)) 2)
(check-expect (depth '(foo 2)) 2)
(check-expect (depth '((foo))) 3)
(check-expect (depth '(2 (3) 2 2 2)) 3)
(check-expect (depth '(2 (3 (4 (5) 4) 3) 2)) 5)

(define (depth sexp)
  (cond
    [(or (atom? sexp) (empty? sexp)) 1]
    [else (add1 (argmax identity (map depth sexp)))]))
