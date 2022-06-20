;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |166|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct empl [name num])
; A Empl is a structure:
;   (make-empl String Number)
; interp. represents an employee with a name and an employee number

(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
;   (make-work Empl Number Number)
; interp. (make-work e r h) combines the employee info with the pay rate r and the number of hours h

; Low (short for list of works) is one of:
; - '()
; - (cons Work Low)
; interp. an instance of Low represents the hours worked for a number of employees

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
;   (make-paycheck Empl Number)
; interp. combines employee and amount to be paid

; Lop (short for list of paychecks) is one of:
; - '()
; - (cons Paycheck Lop)
; interp. an instance of Lop represents paychecks for employees

; Work -> Paycheck
; compute paycheck for the given work
(check-expect (work->paycheck (make-work (make-empl "Joe" 1) 33.33 25))
              (make-paycheck (make-empl "Joe" 1) (* 33.33 25)))

(define (work->paycheck w)
  (make-paycheck (work-employee w) (* (work-rate w) (work-hours w))))

; Low -> Lop
; compute paychecks
(check-expect (wage*.v3 '()) '())
(check-expect (wage*.v3 (cons (make-work (make-empl "Robbie" 2) 11.95 39) '()))
              (cons (make-paycheck (make-empl "Robbie" 2) (* 11.95 39)) '()))
(check-expect (wage*.v3 (cons (make-work (make-empl "Bob" 3) 12 30) (cons (make-work (make-empl "Robbie" 4) 11.95 39) '())))
              (cons (make-paycheck (make-empl "Bob" 3) (* 12 30)) (cons (make-paycheck (make-empl "Robbie" 4) (* 11.95 39)) '())))

(define (wage*.v3 low)
  (cond
    [(empty? low) '()]
    [else (cons (work->paycheck (first low)) (wage*.v3 (rest low)))]))
