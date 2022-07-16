;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 470-gauss) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

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
(define (solve tm)
  (foldr (lambda (e s)
           (cond
             [(empty? s) (cons (/ (second e) (first e)) s)]
             [else (cons (/ (- (rhs e) (plug-in (rest (lhs e)) s)) (first e))
                         s)]))
         '()
         tm))

; Equation Equation -> Equation
; subtracts a multiple of e2 from e1, item by item, so that
; the resulting equation has a 0 in the first position
; assume: e1 and e2 are of equal length
(define (subtract e1 e2)
  (local ((define c (/ (first e1) (first e2))))
    (for/list ([i (rest e1)] [j (rest e2)])
      (- i (* c j)))))

; NEList -> NEList
; moves the first item to the end of the list
(define (rotate-list l)
  (append (rest l) (list (first l))))

; SOE -> Boolean
; return #true if the leading coefficients of all the equations are zeros
(define (leading-zeroes? soe)
  (andmap (lambda (e) (zero? (first e))) soe))

; SOE -> TM
; triangulates the given system of equations
(define (triangulate soe)
  (local ((define e1 (first soe)))
    (cond
      [(= (length soe) 1) soe]
      [(leading-zeroes? soe) (error "no solution")]
      [(zero? (first e1)) (triangulate (rotate-list soe))]
      [else (cons e1 (triangulate (for/list ([i (rest soe)])
                                    (subtract i e1))))])))

; SOE -> Solution
; find a solution to the system of linear equations using gaussian elimination
(check-expect (gauss '((3 4))) '(4/3))
(check-expect (gauss '((2 2 3 10)
                       (2 5 12 31)
                       (4 1 -2 1)))
              '(1 1 2))
(check-expect (gauss '((2 3 3 8)
                       (2 3 -2 3)
                       (4 -2 2 4)))
              '(1 1 1))
(check-error (gauss '((2 2 2 6)
                      (2 2 4 8)
                      (2 2 1 2)))
             "no solution")

(define (gauss soe)
  (solve (triangulate soe)))
