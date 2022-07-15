;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 462-check-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Matrix is one of:
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
;  – '()
;  – (cons Number Row)

; An SOE is a non-empty Matrix.
; constraint: for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation: represents a system of linear equations

; An Equation is a [List-of Number].
; constraint: an Equation contains at least two numbers.
; interpretation: if (list a1 ... an b) is an Equation,
; a1, ..., an are the left-hand-side variable coefficients
; and b is the right-hand side

; A Solution is a [List-of Number]

(define M ; an SOE
  (list (list 2 2  3 10) ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; [List-of Number] Solution -> Number
; calculate the value of the left-hand side when the numbers
; from the solution are plugged in for the variables
(check-expect (plug-in (lhs (first M)) S) 10)
(check-expect (plug-in (lhs (second M)) S) 31)
(check-expect (plug-in (lhs (third M)) S) 1)

(define (plug-in lhs sol)
  (cond
    [(empty? lhs) 0]
    [else (+ (* (first lhs) (first sol))
             (plug-in (rest lhs) (rest sol)))]))

; SOE Solution -> Boolean
; return #true if plugging in the numbers from the Solution
; for the variables in the Equations of the SOE produces equal
; left-hand-side values and right-hand-side values
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 1 3)) #false)

(define (check-solution soe sol)
  (cond
    [(empty? soe) #true]
    [else (local ((define e (first soe)))
            (and (= (plug-in (lhs e) sol) (rhs e))
                 (check-solution (rest soe) sol)))]))
