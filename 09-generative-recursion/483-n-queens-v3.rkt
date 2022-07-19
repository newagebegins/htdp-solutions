;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 483-n-queens-v3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation: (make-posn c r) denotes the square at
; the r-th row and c-th column

(define-struct square [x y threatened?])
; A Square is a structure:
;   (make-square N N Boolean)
; interpretation: a square on a chess board

; A Board is a [List-of Square]

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

; N -> Board
; creates the initial n by n board
(check-expect (board0 1) (list (make-square 0 0 #f)))
(check-expect (board0 2) (list (make-square 0 0 #f) (make-square 1 0 #f)
                               (make-square 0 1 #f) (make-square 1 1 #f)))
(check-expect (board0 3) (list (make-square 0 0 #f) (make-square 1 0 #f) (make-square 2 0 #f)
                               (make-square 0 1 #f) (make-square 1 1 #f) (make-square 2 1 #f)
                               (make-square 0 2 #f) (make-square 1 2 #f) (make-square 2 2 #f)))
(check-expect (board0 4) (list (make-square 0 0 #f) (make-square 1 0 #f) (make-square 2 0 #f) (make-square 3 0 #f)
                               (make-square 0 1 #f) (make-square 1 1 #f) (make-square 2 1 #f) (make-square 3 1 #f)
                               (make-square 0 2 #f) (make-square 1 2 #f) (make-square 2 2 #f) (make-square 3 2 #f)
                               (make-square 0 3 #f) (make-square 1 3 #f) (make-square 2 3 #f) (make-square 3 3 #f)))

(define (board0 n)
  (for*/list ([r n] [c n])
    (make-square c r #false)))

; Board QP -> Board
; places a queen at qp on a-board
(check-expect (add-queen (board0 1) (make-posn 0 0))
              (list (make-square 0 0 #t)))
(check-expect (add-queen (board0 2) (make-posn 1 1))
              (list (make-square 0 0 #t) (make-square 1 0 #t)
                    (make-square 0 1 #t) (make-square 1 1 #t)))

; 012
;0Xoo
;1oo.
;2o.o
(check-expect (add-queen (board0 3) (make-posn 0 0))
              (list (make-square 0 0 #t) (make-square 1 0 #t) (make-square 2 0 #t)
                    (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #f)
                    (make-square 0 2 #t) (make-square 1 2 #f) (make-square 2 2 #t)))

; 012
;0ooo
;1oXo
;2ooo
(check-expect (add-queen (board0 3) (make-posn 1 1))
              (list (make-square 0 0 #t) (make-square 1 0 #t) (make-square 2 0 #t)
                    (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #t)
                    (make-square 0 2 #t) (make-square 1 2 #t) (make-square 2 2 #t)))

; 0123
;0o.o.
;1oo..
;2Xooo
;3oo..
(check-expect (add-queen (board0 4) (make-posn 0 2))
              (list (make-square 0 0 #t) (make-square 1 0 #f) (make-square 2 0 #t) (make-square 3 0 #f)
                    (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #f) (make-square 3 1 #f)
                    (make-square 0 2 #t) (make-square 1 2 #t) (make-square 2 2 #t) (make-square 3 2 #t)
                    (make-square 0 3 #t) (make-square 1 3 #t) (make-square 2 3 #f) (make-square 3 3 #f)))

; 0123
;0oXoo
;1ooo.
;2Xooo
;3oo..
(check-expect (add-queen (list (make-square 0 0 #t) (make-square 1 0 #f) (make-square 2 0 #t) (make-square 3 0 #f)
                               (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #f) (make-square 3 1 #f)
                               (make-square 0 2 #t) (make-square 1 2 #t) (make-square 2 2 #t) (make-square 3 2 #t)
                               (make-square 0 3 #t) (make-square 1 3 #t) (make-square 2 3 #f) (make-square 3 3 #f))
                         (make-posn 1 0))
              (list (make-square 0 0 #t) (make-square 1 0 #t) (make-square 2 0 #t) (make-square 3 0 #t)
                    (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #t) (make-square 3 1 #f)
                    (make-square 0 2 #t) (make-square 1 2 #t) (make-square 2 2 #t) (make-square 3 2 #t)
                    (make-square 0 3 #t) (make-square 1 3 #t) (make-square 2 3 #f) (make-square 3 3 #f)))

(define (add-queen a-board qp)
  (map (lambda (s)
         (make-square (square-x s)
                      (square-y s)
                      (or (square-threatened? s)
                          (threatening? (make-posn (square-x s) (square-y s))
                                        qp))))
       a-board))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(check-expect (find-open-spots (list (make-square 0 0 #t) (make-square 1 0 #t) (make-square 2 0 #t)
                                     (make-square 0 1 #t) (make-square 1 1 #t) (make-square 2 1 #f)
                                     (make-square 0 2 #t) (make-square 1 2 #f) (make-square 2 2 #t)))
              (list (make-posn 2 1) (make-posn 1 2)))

(define (find-open-spots a-board)
  (local (; [List-of Square]
          (define safe-squares (filter (lambda (s)
                                 (false? (square-threatened? s)))
                               a-board))
          ; [List-of QP]
          (define safe-qps (map (lambda (s)
                                  (make-posn (square-x s) (square-y s)))
                                safe-squares)))
    safe-qps))

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(zero? n) '()]
    [else
     (local (; [List-of QP] -> [Maybe [List-of QP]]
             (define (try-place spots)
               (cond
                 [(empty? spots) #false]
                 [else
                  (local (; [Maybe [List-of QP]]
                          (define qps (place-queens (add-queen a-board (first spots))
                                                    (sub1 n))))
                    (if (false? qps)
                        (try-place (rest spots))
                        (cons (first spots) qps)))])))
       (try-place (find-open-spots a-board)))]))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem
(check-satisfied (n-queens 1) (n-queens-solution? 1))
(check-expect (n-queens 2) #false)
(check-expect (n-queens 3) #false)
(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(check-satisfied (n-queens 6) (n-queens-solution? 6))
(check-satisfied (n-queens 7) (n-queens-solution? 7))
(check-satisfied (n-queens 8) (n-queens-solution? 8))

(define (n-queens n)
  (place-queens (board0 n) n))
