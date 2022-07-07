;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 397-wages) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee [name number pay-rate])
; An Employee is a structure:
;   (make-employee String Number Number)

(define-struct time-card [employee-number hours])
; A TimeCard is a structure:
;   (make-time-card Number Number)

(define-struct wage [name amount])
; A Wage is a structure:
;   (make-wage String Number)

(define E1 (make-employee "Alice" 1 100))
(define TC1 (make-time-card 1 39.5))
(define W1 (make-wage "Alice" (* 100 39.5)))

(define E2 (make-employee "Bob" 2 90))
(define TC2 (make-time-card 2 38))
(define W2 (make-wage "Bob" (* 90 38)))

(define E3 (make-employee "Carl" 3 100.5))
(define TC3 (make-time-card 3 42.9))
(define W3 (make-wage "Carl" (* 100.5 42.9)))

(define MISSING-TIME-CARD "missing time card")

; Employee TimeCard -> Wage
(check-expect (calc-wage E1 TC1) W1)
(check-expect (calc-wage E2 TC2) W2)

(define (calc-wage e tc)
  (make-wage (employee-name e) (* (employee-pay-rate e) (time-card-hours tc))))

; Employee [List-of TimeCard] -> TimeCard
(check-expect (find-time-card E1 (list TC1 TC2)) TC1)
(check-expect (find-time-card E1 (list TC2 TC1)) TC1)
(check-expect (find-time-card E2 (list TC3 TC2 TC1)) TC2)
(check-error (find-time-card E1 '()) MISSING-TIME-CARD)
(check-error (find-time-card E1 (list TC2 TC3)) MISSING-TIME-CARD)

(define (find-time-card e lotc)
  (cond
    [(empty? lotc) (error MISSING-TIME-CARD)]
    [else (local ((define tc (first lotc)))
            (if (= (time-card-employee-number tc) (employee-number e))
                tc
                (find-time-card e (rest lotc))))]))

; [List-of Employee] [List-of TimeCard] -> [List-of Wage]
; calculate employee wages
; assumption: there is at most one time card per employee number
(check-expect (wages*.v3 (list E1 E2 E3) (list TC2 TC3 TC1)) (list W1 W2 W3))
(check-error (wages*.v3 (list E1 E2) (list TC1)) MISSING-TIME-CARD)

(define (wages*.v3 loe lotc)
  (cond
    [(empty? loe) '()]
    [else (cons (calc-wage (first loe) (find-time-card (first loe) lotc))
                (wages*.v3 (rest loe) lotc))]))
