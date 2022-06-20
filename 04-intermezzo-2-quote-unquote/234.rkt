;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |234|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-strings -> ... nested list ...
(check-expect (add-ranks '()) '())
(check-expect (add-ranks '("c" "b" "a")) '((3 "c") (2 "b") (1 "a")))

(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-strings -> ... nested list ...
(check-expect (ranking '()) '())
(check-expect (ranking '("a" "b" "c")) '((1 "a") (2 "b") (3 "c")))

(define (ranking los)
  (reverse (add-ranks (reverse los))))

(check-expect (make-row '(1 "a")) '(tr (td "1") (td "a")))

(define (make-row r)
  `(tr (td ,(number->string (first r)))
       (td ,(second r))))

(check-expect (make-rows '()) '())
(check-expect (make-rows '((1 "a") (2 "b") (3 "c")))
              '((tr (td "1") (td "a"))
                (tr (td "2") (td "b"))
                (tr (td "3") (td "c"))))

(define (make-rows lor)
  (cond
    [(empty? lor) '()]
    [else (cons (make-row (first lor)) (make-rows (rest lor)))]))

; List-of-strings -> ... nested list ...
; produces a list representation of an HTML table with ranked song titles
(check-expect (make-ranking '("a" "b" "c"))
              '(table (tr (td "1") (td "a"))
                      (tr (td "2") (td "b"))
                      (tr (td "3") (td "c"))))

(define (make-ranking los)
  `(table ,@(make-rows (ranking los))))
