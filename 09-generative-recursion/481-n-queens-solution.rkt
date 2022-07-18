;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 481-n-queens-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn c r) denotes the square at
; the r-th row and c-th column

; QP QP -> Boolean
; return #true if the queens on the squares qp1 and qp2 threaten each other
(define (threatening? qp1 qp2)
  (local ((define hor (= (posn-y qp1) (posn-y qp2)))
          (define ver (= (posn-x qp1) (posn-x qp2)))
          (define diag1 (= (+ (posn-x qp1) (posn-y qp1))
                           (+ (posn-x qp2) (posn-y qp2))))
          (define diag2 (= (- (posn-x qp1) (posn-y qp1))
                           (- (posn-x qp2) (posn-y qp2)))))
    (or hor ver diag1 diag2)))

; N -> [[List-of QP] -> Boolean]
; check that solution is correct

; 0123
;0..X.
;1X...
;2...X
;3.X..
(check-expect ((n-queens-solution? 4)
               (list (make-posn 2 0) (make-posn 0 1) (make-posn 3 2) (make-posn 1 3)))
              #true)

; 0123
;0..X.
;1X...
;2..X.
;3.X..
(check-expect ((n-queens-solution? 4)
               (list (make-posn 2 0) (make-posn 0 1) (make-posn 2 2) (make-posn 2 3)))
              #false)

; 01234
;0X....
;1..X..
;2....X
;3.X...
;4...X.
(check-expect ((n-queens-solution? 5)
               (list (make-posn 0 0) (make-posn 1 3) (make-posn 2 1) (make-posn 3 4) (make-posn 4 2)))
              #true)

; 01234
;0X....
;1..X..
;2.X..X
;3.....
;4...X.
(check-expect ((n-queens-solution? 5)
               (list (make-posn 0 0) (make-posn 1 2) (make-posn 2 1) (make-posn 3 4) (make-posn 4 2)))
              #false)

; 01234
;0X....
;1..X..
;2.....
;3.X...
;4...X.
(check-expect ((n-queens-solution? 5)
               (list (make-posn 0 0) (make-posn 1 3) (make-posn 2 1) (make-posn 3 4)))
              #false)

(define (n-queens-solution? n)
  (local (; [List-of QP] -> Boolean
          (define (threatening?/list qps)
            (cond
              [(empty? qps) #false]
              [else (or (ormap (lambda (qp)
                                 (threatening? (first qps) qp))
                               (rest qps))
                        (threatening?/list (rest qps)))])))
  (lambda (qps)
    (and (= (length qps) n)
         (not (threatening?/list qps))))))
