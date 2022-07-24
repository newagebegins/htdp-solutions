;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 517-static-distance) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of:
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

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

; [X] X [List-of X] -> N
; returns the index of it in l
(check-expect (index 'a '(a b c)) 0)
(check-expect (index 'b '(a b c)) 1)
(check-expect (index 'c '(a b c)) 2)
(check-error (index 'd '(a b c)) "not found")

(define (index it l)
  (cond
    [(empty? l) (error "not found")]
    [else (if (equal? (first l) it)
              0
              (add1 (index it (rest l))))]))

; Lam -> Lam
; replaces all occurrences of variables with a natural number that represents how far away the declaring λ is
(check-expect (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))

(define (static-distance le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator: declareds is a list of all λ parameters on the path from le0 to le
          (define (sd/a le declareds)
            (cond
              [(is-var? le) (index le declareds)]
              [(is-λ? le) (local ((define para (λ-para le))
                                  (define body (λ-body le)))
                            (list 'λ (list para)
                                  (sd/a body (cons para declareds))))]
              [(is-app? le) (list (sd/a (app-fun le) declareds)
                                  (sd/a (app-arg le) declareds))])))
    (sd/a le0 '())))
