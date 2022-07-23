;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |512|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of:
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

; Lam -> Boolean
(check-expect (is-var? 'foo) #true)
(check-expect (is-var? '(λ (x) x)) #false)
(check-expect (is-var? '(f x)) #false)
(check-expect (is-var? '((λ (x) x) (λ (x) x))) #false)

(define is-var? symbol?)

; Lam -> Boolean
(check-expect (is-λ? 'foo) #false)
(check-expect (is-λ? '(λ (x) x)) #true)
(check-expect (is-λ? '(f x)) #false)
(check-expect (is-λ? '((λ (x) x) (λ (x) x))) #false)

(define (is-λ? lam)
  (and (list? lam)
       (equal? (first lam) 'λ)))

; Lam -> Boolean
(check-expect (is-app? 'foo) #false)
(check-expect (is-app? '(λ (x) x)) #false)
(check-expect (is-app? '(f x)) #true)
(check-expect (is-app? '((λ (x) x) (λ (x) x))) #true)

(define (is-app? lam)
  (and (list? lam)
       (not (equal? (first lam) 'λ))))

; Lam -> Symbol
; extracts the parameter from a λ expressiion
(check-expect (λ-para '(λ (x) x)) 'x)
(check-expect (λ-para '(λ (y) (λ (x) y))) 'y)

(define (λ-para lam)
  (first (second lam)))

; Lam -> Lam
; extracts the body from a λ expression
(check-expect (λ-body '(λ (x) x)) 'x)
(check-expect (λ-body '(λ (y) (λ (x) y))) '(λ (x) y))

(define (λ-body lam)
  (third lam))

; Lam -> Lam
; extracts the function from an application
(check-expect (app-fun '(f x)) 'f)
(check-expect (app-fun '((λ (x) x) (λ (y) y))) '(λ (x) x))

(define (app-fun lam)
  (first lam))

; Lam -> Lam
; extracts the argument from an application
(check-expect (app-arg '(f x)) 'x)
(check-expect (app-arg '((λ (x) x) (λ (y) y))) '(λ (y) y))

(define (app-arg lam)
  (second lam))

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters in a λ term
(check-expect (declareds '(λ (x) x)) '(x))
(check-expect (declareds '(λ (y) (λ (x) y))) '(y x))
(check-expect (declareds '(λ (y) (λ (x) (λ (z) y)))) '(y x z))
(check-expect (declareds '(λ (x) (x (λ (y) z)))) '(x y))
(check-expect (declareds '(λ (x) ((λ (w) (λ (v) z)) (λ (y) y)))) '(x w v y))

(define (declareds lam)
  (cond
    [(is-var? lam) '()]
    [(is-λ? lam) (cons (λ-para lam) (declareds (λ-body lam)))]
    [(is-app? lam) (append (declareds (app-fun lam))
                           (declareds (app-arg lam)))]))
