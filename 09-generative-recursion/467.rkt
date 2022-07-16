;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |467|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Equation Equation -> Equation
; subtracts a multiple of e2 from e1, item by item, so that
; the resulting equation has a 0 in the first position
; assume: e1 and e2 are of equal length
(check-expect (subtract '(2 5 12 31) '(2 2 3 10)) '(3 9 21))
(check-expect (subtract '(4 1 -2 1) '(2 2 3 10)) '(-3 -8 -19))
(check-expect (subtract '(2 5 9) '(4 8 10)) '(1 4))

(define (subtract e1 e2)
  (local ((define c (/ (first e1) (first e2))))
    (for/list ([i (rest e1)] [j (rest e2)])
      (- i (* c j)))))

; NEList -> NEList
; moves the first item to the end of the list
(check-expect (rotate-list '(1 2 3)) '(2 3 1))

(define (rotate-list l)
  (append (rest l) (list (first l))))

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate '((3 4))) '((3 4)))
(check-expect (triangulate '((2 2 3 10)
                             (2 5 12 31)
                             (4 1 -2 1)))
              '((2 2 3 10)
                (3 9 21)
                (1 2)))
(check-expect (triangulate '((2 3 3 8)
                             (2 3 -2 3)
                             (4 -2 2 4)))
              '((2 3 3 8)
                (-8 -4 -12)
                (-5 -5)))

(define (triangulate soe)
  (local ((define e1 (first soe)))
    (cond
      [(= (length soe) 1) soe]
      [(zero? (first e1)) (triangulate (rotate-list soe))]
      [else (cons e1 (triangulate (for/list ([i (rest soe)])
                                    (subtract i e1))))])))
