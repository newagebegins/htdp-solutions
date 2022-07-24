;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |516|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of:
; – Symbol
; - Lambda
; - Application

(define-struct lmb [para body])
; A Lambda is a structure:
;   (make-lmb Symbol Lam)

(define-struct app [fun arg])
; An Application is a structure:
;   (make-app Lam Lam)

; Lam -> Boolean
(define var? symbol?)

; Lam -> Lam
; replaces all symbols s in le with:
; - '(*undeclared s) if they do not occur within the body of a λ expression whose parameter is s
; - '(*declared s) if they occur within the body of a λ expression whose parameter is s

(check-expect (undeclareds (make-lmb 'x 'x)) (make-lmb 'x '(*declared x)))
(check-expect (undeclareds (make-lmb 'x 'y)) (make-lmb 'x '(*undeclared y)))
(check-expect (undeclareds (make-lmb 'y (make-lmb 'x 'y)))
              (make-lmb 'y (make-lmb 'x '(*declared y))))
(check-expect (undeclareds (make-app (make-lmb 'x (make-app 'x 'x))
                                     (make-lmb 'x (make-app 'x 'x))))
              (make-app (make-lmb 'x (make-app '(*declared x) '(*declared x)))
                        (make-lmb 'x (make-app '(*declared x) '(*declared x)))))
(check-expect (undeclareds (make-app (make-lmb 'x (make-app 'x 'x))
                                     (make-lmb 'y (make-app 'y 'x))))
              (make-app (make-lmb 'x (make-app '(*declared x) '(*declared x)))
                        (make-lmb 'y (make-app '(*declared y) '(*undeclared x)))))
(check-expect (undeclareds (make-lmb '*undeclared
                                     (make-app (make-lmb 'x
                                                         (make-app 'x '*undeclared))
                                               'y)))
              (make-lmb '*undeclared
                        (make-app (make-lmb 'x
                                            (make-app '(*declared x) '(*declared *undeclared)))
                                  '(*undeclared y))))

(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator: declareds is a list of all λ parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(var? le)
               (if (member? le declareds)
                   (list '*declared le)
                   (list '*undeclared le))]
              [(lmb? le)
               (local ((define para (lmb-para le))
                       (define body (lmb-body le))
                       (define newd (cons para declareds)))
                 (make-lmb para (undeclareds/a body newd)))]
              [(app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (make-app (undeclareds/a fun declareds)
                           (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))
