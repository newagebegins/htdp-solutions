;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 469-solve-challenge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Matrix is one of:
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint: all rows in matrix are of the same length

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

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;   n + 1, n, n - 1, ..., 2.
; interpretation: represents a triangular matrix

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(define (rhs e)
  (first (reverse e)))

; [List-of Number] Solution -> Number
; calculate the value of the left-hand side when the numbers
; from the solution are plugged in for the variables
(define (plug-in lhs sol)
  (cond
    [(empty? lhs) 0]
    [else (+ (* (first lhs) (first sol))
             (plug-in (rest lhs) (rest sol)))]))

; TM -> Solution
; produces a solution for a triangular SOE
(check-expect (solve '((2 2 3 10)
                       (  3 9 21)
                       (    1  2)))
              '(1 1 2))
(check-expect (solve '((2  3  3   8)
                       (  -8 -4 -12)
                       (     -5  -5)))
              '(1 1 1))

(define (solve tm)
  (foldr (lambda (e s)
           (cond
             [(empty? s) (cons (/ (second e) (first e)) s)]
             [else (cons (/ (- (rhs e) (plug-in (rest (lhs e)) s)) (first e))
                         s)]))
         '()
         tm))
