;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |176|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Matrix is one of:
; - (cons Row '())
; - (cons Row Matrix)
; constraint: all rows in matrix are of the same length

; A Row is one of:
; - '()
; - (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(define row3 (cons 11 (cons 12 (cons 13 '()))))
(define row4 (cons 21 (cons 22 (cons 23 '()))))
(define row5 (cons 31 (cons 32 (cons 33 '()))))
(define mat2 (cons row3 (cons row4 (cons row5 '()))))

(define wor3 (cons 11 (cons 21 (cons 31 '()))))
(define wor4 (cons 12 (cons 22 (cons 32 '()))))
(define wor5 (cons 13 (cons 23 (cons 33 '()))))
(define tam2 (cons wor3 (cons wor4 (cons wor5 '()))))

; Matrix -> Row
; produce the first column of a matrix as a list of numbers
(check-expect (first* (cons '() '())) '())
(check-expect (first* (cons (cons 5 '()) '())) (cons 5 '()))
(check-expect (first* (cons (cons 5 '()) (cons (cons 6 '()) '()))) (cons 5 (cons 6 '())))
(check-expect (first* mat1) (cons 11 (cons 21 '())))

(define (first* lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first (first lln))
                (if (empty? (rest lln))
                    '()
                    (first* (rest lln))))]))

; Matrix -> Matrix
; consumes a matrix and remove the first column
(check-expect (rest* (cons '() '())) (cons '() '()))
(check-expect (rest* (cons (cons 1 '()) '())) (cons '() '()))
(check-expect (rest* (cons (cons 1 (cons 2 '())) '())) (cons (cons 2 '()) '()))
(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 22 '()) '())))

(define (rest* lln)
  (cond
    [(empty? (first lln)) (cons '() '())]
    [else (cons (rest (first lln))
                (if (empty? (rest lln))
                    '()
                    (rest* (rest lln))))]))

; Matrix -> Matrix
; transposes the given matrix along the diagonal
(check-expect (transpose (cons (cons 5 '()) '())) (cons (cons 5 '()) '()))
(check-expect (transpose mat1) tam1)
(check-expect (transpose mat2) tam2)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))
