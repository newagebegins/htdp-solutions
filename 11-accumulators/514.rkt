;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |514|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of:
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Boolean
(define is-var? symbol?)

; Lam -> Boolean
(define (is-λ? lam)
  (and (list? lam)
       (equal? (first lam) 'λ)))

; Lam -> Boolean
(define (is-app? lam)
  (and (list? lam)
       (not (equal? (first lam) 'λ))))

; Lam -> Symbol
; extracts the parameter from a λ expressiion
(define (λ-para lam)
  (first (second lam)))

; Lam -> Lam
; extracts the body from a λ expression
(define (λ-body lam)
  (third lam))

; Lam -> Lam
; extracts the function from an application
(define (app-fun lam)
  (first lam))

; Lam -> Lam
; extracts the argument from an application
(define (app-arg lam)
  (second lam))

; Lam -> Lam
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ
; expression whose parameter is s

(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
; in the first lambda the x is bound, in the second - free; the function works properly
(check-expect (undeclareds '((λ (x) (x x)) (λ (y) (y x))))
              '((λ (x) (x x)) (λ (y) (y *undeclared))))

(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))
