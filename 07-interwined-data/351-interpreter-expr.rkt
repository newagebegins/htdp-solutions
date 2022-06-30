;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 351-interpreter-expr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

(define WRONG "parse error")

; S-expr -> Number
(check-expect (interpreter-expr 1) 1)
(check-error (interpreter-expr "foo"))
(check-error (interpreter-expr 'foo))
(check-expect (interpreter-expr '(+ 2 3)) (+ 2 3))
(check-expect (interpreter-expr '(* 2 3)) (* 2 3))
(check-error (interpreter-expr '(/ 10 2)))
(check-error (interpreter-expr '(+ 1 2 3)))

(define (interpreter-expr s)
  (eval-expression (parse s)))

; BSL-expr -> Number
; compute the value of a BSL expression
(define (eval-expression expr)
  (cond
    [(number? expr) expr]
    [(add? expr) (+ (eval-expression (add-left expr))
                    (eval-expression (add-right expr)))]
    [(mul? expr) (* (eval-expression (mul-left expr))
                    (eval-expression (mul-right expr)))]))

; S-expr -> BSL-expr
(check-expect (parse 1) 1)
(check-error (parse "foo"))
(check-error (parse 'foo))
(check-expect (parse '(+ 2 3)) (make-add 2 3))
(check-expect (parse '(* 2 3)) (make-mul 2 3))
(check-error (parse '(/ 10 2)))
(check-error (parse '(+ 1 2 3)))

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; Any -> Boolean
; return #true if x is an Atom
(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))
