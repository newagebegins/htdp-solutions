;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 401-sexp-equal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr (S-expression) is one of:
; – Atom
; – [List-of S-expr]

; An Atom is one of:
; – Number
; – String
; – Symbol

; S-expr S-expr -> Boolean
; return #true if the two S-expressions are equal
(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? 1 2) #false)
(check-expect (sexp=? 1 "a") #false)
(check-expect (sexp=? 1 'a) #false)
(check-expect (sexp=? 1 '()) #false)
(check-expect (sexp=? 1 '(1)) #false)

(check-expect (sexp=? "a" 1) #false)
(check-expect (sexp=? "foo" "foo") #true)
(check-expect (sexp=? "foo" "bar") #false)
(check-expect (sexp=? "a" 'a) #false)
(check-expect (sexp=? "a" '()) #false)
(check-expect (sexp=? "a" '("a")) #false)

(check-expect (sexp=? 'foo 'foo) #true)
(check-expect (sexp=? 'foo 'bar) #false)
(check-expect (sexp=? 'a 1) #false)
(check-expect (sexp=? 'a "a") #false)
(check-expect (sexp=? 'a '()) #false)
(check-expect (sexp=? 'a '(a)) #false)

(check-expect (sexp=? '(1) '(1)) #true)
(check-expect (sexp=? '(1) '(2)) #false)
(check-expect (sexp=? '(1) 1) #false)
(check-expect (sexp=? '(1) "1") #false)
(check-expect (sexp=? '(a) 'a) #false)

(check-expect (sexp=? '() '()) #true)
(check-expect (sexp=? '() 1) #false)
(check-expect (sexp=? '() "a") #false)
(check-expect (sexp=? '() 'a) #false)

(check-expect (sexp=? '(1 2) '(1 2)) #true)
(check-expect (sexp=? '(1 2) '(2 1)) #false)
(check-expect (sexp=? '(1 2 3) '(1 2)) #false)
(check-expect (sexp=? '(1 2) '(1 2 3)) #false)
(check-expect (sexp=? '((1 2) (a "foo" (5))) '((1 2) (a "foo" (5)))) #true)
(check-expect (sexp=? '((1 2) (a "foo" (5))) '((1 2) (a "bar" (5)))) #false)

(define (sexp=? s1 s2)
  (cond
    [(and (number? s1) (number? s2)) (= s1 s2)]
    [(and (number? s1) (string? s2)) #false]
    [(and (number? s1) (symbol? s2)) #false]
    [(and (number? s1) (empty? s2)) #false]
    [(and (number? s1) (cons? s2)) #false]
    [(and (string? s1) (number? s2)) #false]
    [(and (string? s1) (string? s2)) (string=? s1 s2)]
    [(and (string? s1) (symbol? s2)) #false]
    [(and (string? s1) (empty? s2)) #false]
    [(and (string? s1) (cons? s2)) #false]
    [(and (symbol? s1) (number? s2)) #false]
    [(and (symbol? s1) (string? s2)) #false]
    [(and (symbol? s1) (symbol? s2)) (symbol=? s1 s2)]
    [(and (symbol? s1) (empty? s2)) #false]
    [(and (symbol? s1) (cons? s2)) #false]
    [(and (cons? s1) (number? s2)) #false]
    [(and (cons? s1) (string? s2)) #false]
    [(and (cons? s1) (symbol? s2)) #false]
    [(and (cons? s1) (empty? s2)) #false]
    [(and (cons? s1) (cons? s2)) (and (sexp=? (first s1) (first s2))
                                      (sexp=? (rest s1) (rest s2)))]
    [(and (empty? s1) (number? s2)) #false]
    [(and (empty? s1) (string? s2)) #false]
    [(and (empty? s1) (symbol? s2)) #false]
    [(and (empty? s1) (empty? s2)) #true]
    [(and (empty? s1) (cons? s2)) #false]))
