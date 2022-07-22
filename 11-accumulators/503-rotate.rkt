;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 503-rotate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Matrix -> Matrix
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 4 5) (0 2 3))) "all rows start with 0")

(define (rotate M)
  (cond
    [(andmap (lambda (row) (zero? (first row))) M) (error "all rows start with 0")]
    [else
     (local ((define (rot M)
               (cond
                 [(not (= (first (first M)) 0)) M]
                 [else
                  (rot (append (rest M) (list (first M))))])))
       (rot M))]))

; N -> Matrix
; create a test matrix with n number of rows
(define (test-matrix n)
  (build-list n (lambda (row)
                  (build-list 3 (if (= row (- n 1))
                                    add1
                                    identity)))))

; Matrix -> Matrix
; finds a row that doesn't start with 0 and uses it as the first one
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate.v2 '((0 4 5) (0 2 3))) "all rows start with 0")
(check-expect (rotate.v2 '((0 4 5) (0 2 3) (1 7 8) (0 5 5)))
              '((1 7 8) (0 5 5) (0 2 3) (0 4 5)))

(define (rotate.v2 M0)
  (local (; Matrix Matrix -> Matrix
          ; accumulator: seen is all the rows in M0 that are not in M
          (define (rotate/a M seen)
            (cond
              [(empty? M) (error "all rows start with 0")]
              [(not (zero? (first (first M)))) (append M seen)]
              [else (rotate/a (rest M) (cons (first M) seen))])))
    (rotate/a M0 '())))

(define M1 (test-matrix 1000))
(define M2 (test-matrix 2000))
(define M3 (test-matrix 3000))
(define M4 (test-matrix 4000))
(define M5 (test-matrix 5000))

(define T1 (time (rotate M1))) ; 10
(define T2 (time (rotate M2))) ; 40
(define T3 (time (rotate M3))) ; 80
(define T4 (time (rotate M4))) ; 180
(define T5 (time (rotate M5))) ; 250

(define U1 (time (rotate.v2 M1))) ; 0
(define U2 (time (rotate.v2 M2))) ; 2
(define U3 (time (rotate.v2 M3))) ; 2
(define U4 (time (rotate.v2 M4))) ; 2
(define U5 (time (rotate.v2 M5))) ; 3
