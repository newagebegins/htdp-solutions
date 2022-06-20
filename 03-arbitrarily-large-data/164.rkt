;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |164|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

(define EUROS-PER-DOLLAR 0.93)

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts.
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (cons 1 '())) (cons EUROS-PER-DOLLAR '()))
(check-expect (convert-euro (cons 0.5 (cons 10 '())))
              (cons (* 0.5 EUROS-PER-DOLLAR)
                    (cons (* 10 EUROS-PER-DOLLAR) '())))

(define (convert-euro l)
  (cond
    [(empty? l) '()]
    [else (cons (* (first l) EUROS-PER-DOLLAR) (convert-euro (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; given an exchange rate converts a list of US$ amounts into a list of € amounts.
(check-expect (convert-euro*  1.2 '()) '())
(check-expect (convert-euro*  1.3 (cons 1 '())) (cons 1.3 '()))
(check-expect (convert-euro* EUROS-PER-DOLLAR (cons 0.5 (cons 10 '())))
              (cons (* 0.5 EUROS-PER-DOLLAR)
                    (cons (* 10 EUROS-PER-DOLLAR) '())))

(define (convert-euro* e l)
  (cond
    [(empty? l) '()]
    [else (cons (* (first l) e) (convert-euro* e (rest l)))]))
