;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |187|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct gp [name score])
; A GamePlayer is a structure:
;   (make-gp String Number)
; interp. (make-gp p s) represents player p who scored a maximum of s points

; A List-of-gp is one of:
; - '()
; - (cons GamePlayer List-of-gp)

; GamePlayer GamePlayer -> Boolean
; return #true when the score of p1 is strictly less than the score of p2
(check-expect (gp<? (make-gp "a" 5) (make-gp "b" 6)) #true)
(check-expect (gp<? (make-gp "a" 6) (make-gp "b" 5)) #false)
(check-expect (gp<? (make-gp "a" 5) (make-gp "b" 5)) #false)

(define (gp<? p1 p2)
  (< (gp-score p1) (gp-score p2)))

; GamePlayer List-of-gp -> List-of-gp
; inserts p into the sorted list of game players l
(check-expect (insert (make-gp "a" 5) '())
              (list (make-gp "a" 5)))
(check-expect (insert (make-gp "a" 5) (list (make-gp "b" 6)))
              (list (make-gp "b" 6) (make-gp "a" 5)))
(check-expect (insert (make-gp "a" 5) (list (make-gp "b" 4)))
              (list (make-gp "a" 5) (make-gp "b" 4)))
(check-expect (insert (make-gp "a" 12) (list (make-gp "b" 20) (make-gp "c" -5)))
              (list (make-gp "b" 20) (make-gp "a" 12) (make-gp "c" -5)))
(check-expect (insert (make-gp "a" 20) (list (make-gp "b" 20) (make-gp "c" -5)))
              (list (make-gp "a" 20) (make-gp "b" 20) (make-gp "c" -5)))
(check-expect (insert (make-gp "a" 4) (list (make-gp "b" 3) (make-gp "c" 2) (make-gp "d" 1)))
              (list (make-gp "a" 4) (make-gp "b" 3) (make-gp "c" 2) (make-gp "d" 1)))
(check-expect (insert (make-gp "a" 0) (list (make-gp "b" 3) (make-gp "c" 2) (make-gp "d" 1)))
              (list (make-gp "b" 3) (make-gp "c" 2) (make-gp "d" 1) (make-gp "a" 0)))

(define (insert p l)
  (cond
    [(empty? l) (list p)]
    [else (if (gp<? p (first l))
              (cons (first l) (insert p (rest l)))
              (cons p l))]))

; List-of-gp -> List-of-gp
; rearranges l in descending order

(check-expect (sort> '()) '())
(check-expect (sort> (list (make-gp "a" 3) (make-gp "b" 2) (make-gp "c" 1)))
              (list (make-gp "a" 3) (make-gp "b" 2) (make-gp "c" 1)))
(check-expect (sort> (list (make-gp "a" 1) (make-gp "b" 2) (make-gp "c" 3)))
              (list (make-gp "c" 3) (make-gp "b" 2) (make-gp "a" 1)))
(check-expect (sort> (list (make-gp "a" 12) (make-gp "b" 20) (make-gp "c" -5)))
              (list (make-gp "b" 20) (make-gp "a" 12) (make-gp "c" -5)))

(define (sort> l)
  (cond
    [(empty? l) '()]
    [else (insert (first l) (sort> (rest l)))]))
