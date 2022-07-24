;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |515|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Lam -> Lam
; replaces all symbols s in le with:
; - '(*undeclared s) if they do not occur within the body of a λ expression whose parameter is s
; - '(*declared s) if they occur within the body of a λ expression whose parameter is s

(check-expect (undeclareds '(λ (x) x)) '(λ (x) (*declared x)))
(check-expect (undeclareds '(λ (x) y)) '(λ (x) (*undeclared y)))
(check-expect (undeclareds '(λ (y) (λ (x) y))) '(λ (y) (λ (x) (*declared y))))
(check-expect (undeclareds '((λ (x) (x x)) (λ (x) (x x)))) '((λ (x) ((*declared x) (*declared x)))
                                                             (λ (x) ((*declared x) (*declared x)))))
(check-expect (undeclareds '((λ (x) (x x)) (λ (y) (y x))))
              '((λ (x) ((*declared x) (*declared x))) (λ (y) ((*declared y) (*undeclared x)))))
(check-expect (undeclareds '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))
              '(λ (*undeclared) ((λ (x) ((*declared x) (*declared *undeclared))) (*undeclared y))))

(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds)
                   (list '*declared le)
                   (list '*undeclared le))]
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
