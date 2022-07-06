;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 388-wages) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee [name ssn pay-rate])
; An Employee is a structure:
;   (make-employee String Number Number)
; interpretation: (make-employee n s p) represents an employee with name n,
; social security number s and hourly pay rate p

(define-struct work-record [name hours])
; A WorkRecord is a structure:
;   (make-work-record String Number)
; interpretation: (make-work-record n h) represents amount of hours h that
; employee with name n has worked

(define-struct weekly-wage [name amount])
; A WeeklyWage is a structure:
;   (make-weekly-wage String Number)
; interpretation: (make-weekly-wage n a) represents the weekly wage a for the employee with name n

(define E1 (make-employee "Alice" 123 100))
(define WR1 (make-work-record "Alice" 40))
(define WW1 (make-weekly-wage "Alice" 4000))

(define E2 (make-employee "Bob" 456 90.5))
(define WR2 (make-work-record "Bob" 38.5))
(define WW2 (make-weekly-wage "Bob" (* 90.5 38.5)))

; Employee WorkRecord -> WeeklyWage
; calculate the weekly wage for the given employee and the work record
(check-expect (wage E1 WR1) WW1)
(check-expect (wage E2 WR2) WW2)

(define (wage e wr)
  (make-weekly-wage (employee-name e) (* (employee-pay-rate e) (work-record-hours wr))))

; [List-of Employee] [List-of WorkRecord] -> [List-of WeeklyWage]
; produce weekly wages for the given lists of employees and work records
; assume the lists have equal length and
; the Nth item in employees corresponds to the Nth item in work-records
(check-expect (wages* '() '()) '())
(check-expect (wages* (list E1) (list WR1)) (list WW1))
(check-expect (wages* (list E1 E2) (list WR1 WR2)) (list WW1 WW2))

(define (wages* employees work-records)
  (cond
    [(empty? employees) '()]
    [else (cons (wage (first employees) (first work-records))
                (wages* (rest employees) (rest work-records)))]))
