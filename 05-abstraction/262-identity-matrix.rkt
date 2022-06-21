;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 262-identity-matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Row is [List-of Number]
; Matrix is [List-of Row]

; Matrix -> Matrix
; adds zeros at the end of every row
(check-expect (add-zeros-at-end '()) '())
(check-expect (add-zeros-at-end '((1))) '((1 0)))
(check-expect (add-zeros-at-end '((1 0) (0 1))) '((1 0 0) (0 1 0)))
(check-expect (add-zeros-at-end '((1 0 0) (0 1 0) (0 0 1))) '((1 0 0 0) (0 1 0 0) (0 0 1 0)))

(define (add-zeros-at-end rows)
  (cond
    [(empty? rows) '()]
    [else (cons (append (first rows) '(0))
                (add-zeros-at-end (rest rows)))]))

; N -> Row
; creates the last row of an identity matrix of the given dimension
(check-expect (last-identity-row 1) '(1))
(check-expect (last-identity-row 2) '(0 1))
(check-expect (last-identity-row 3) '(0 0 1))
(check-expect (last-identity-row 4) '(0 0 0 1))

(define (last-identity-row d)
  (cond
    [(= d 1) '(1)]
    [else (cons 0 (last-identity-row (sub1 d)))]))

; N -> Matrix
; creates an identity matrix of the given dimension

(check-expect (identityM 0) '())

(check-expect (identityM 1) '((1)))

(check-expect (identityM 2) '((1 0)
                              (0 1)))

(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))

(check-expect (identityM 4) '((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 1)))

(define (identityM d)
  (cond
    [(zero? d) '()]
    [else (append (add-zeros-at-end (identityM (sub1 d)))
                  (list (last-identity-row d)))]))
