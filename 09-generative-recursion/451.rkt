;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |451|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; [List-of Number] -> Table
(define (test-table l)
  (make-table (length l) (lambda (i) (list-ref l i))))

(define L1 '(-4 -3.5 -2.1 -1 0.2 1 2.5 4 5 10))
(define R1 4)
(define T1 (test-table L1))

(define L2 '(0 10 20 30))
(define R2 0)
(define T2 (test-table L2))

(define L3 '(0.1 10 20 30))
(define R3 0)
(define T3 (test-table L3))

(define L4 '(5 10 20 30))
(define R4 0)
(define T4 (test-table L4))

(define L5 '(-30 -20 -10 -0.1))
(define R5 3)
(define T5 (test-table L5))

(define L6 '(-4 -3.5 -2.1 -1 -0.5 -0.3 0 1 2.5 4 5 10))
(define R6 6)
(define T6 (test-table L6))

(define L7 '(-1 2))
(define R7 0)
(define T7 (test-table L7))

(define L8 '(-1 0.1))
(define R8 1)
(define T8 (test-table L8))

(define L9 '(5))
(define R9 0)
(define T9 (test-table L9))

(define L10 '(-3 -2 -1 0 1 2 3))
(define R10 3)
(define T10 (test-table L10))

; Table -> N
; finds the smallest index for a root of the monotonically increasing table t
(check-expect (find-linear T1) R1)
(check-expect (find-linear T2) R2)
(check-expect (find-linear T3) R3)
(check-expect (find-linear T4) R4)
(check-expect (find-linear T5) R5)
(check-expect (find-linear T6) R6)
(check-expect (find-linear T7) R7)
(check-expect (find-linear T8) R8)
(check-expect (find-linear T9) R9)
(check-expect (find-linear T10) R10)

(define (find-linear t)
  (local (; N -> N
          (define (helper i)
            (cond
              [(= (add1 i) (table-length t)) i]
              [else (if (<= (abs (table-ref t i)) (abs (table-ref t (add1 i))))
                        i
                        (helper (add1 i)))])))
    (helper 0)))

; Table -> N
; finds the smallest index for a root of the monotonically increasing table t
; termination: each recursion step we deal with a smaller interval
(check-expect (find-binary T1) R1)
(check-expect (find-binary T2) R2)
(check-expect (find-binary T3) R3)
(check-expect (find-binary T4) R4)
(check-expect (find-binary T5) R5)
(check-expect (find-binary T6) R6)
(check-expect (find-binary T7) R7)
(check-expect (find-binary T8) R8)
(check-expect (find-binary T9) R9)
(check-expect (find-binary T10) R10)

(define (find-binary t)
  (local (; N N -> N
          (define (helper l r)
            (local ((define m (quotient (+ l r) 2))
                    (define vl (table-ref t l))
                    (define vr (table-ref t r))
                    (define vm (table-ref t m)))
              (cond
                [(= l r) l]
                [(= (add1 l) r) (if (< (abs vl) (abs vr))
                                    l
                                    r)]
                [(<= vl vr 0) r]
                [(<= 0 vl vr) l]
                [(<= vl 0 vr) (if (<= vm 0)
                                  (helper m r)
                                  (helper l m))]))))
    (helper 0 (sub1 (table-length t)))))
