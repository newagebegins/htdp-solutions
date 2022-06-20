;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |169|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; - (cons Posn '())
; - (cons Posn List-of-posns)
; interp. a list of positions

; Posn -> Boolean
; returns #true for posns whose x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200
(check-expect (legal? (make-posn 0 0)) #true)
(check-expect (legal? (make-posn 50 100)) #true)
(check-expect (legal? (make-posn 100 200)) #true)

(check-expect (legal? (make-posn -1 0)) #false)
(check-expect (legal? (make-posn 101 0)) #false)
(check-expect (legal? (make-posn 0 -1)) #false)
(check-expect (legal? (make-posn 0 201)) #false)
(check-expect (legal? (make-posn -1 -1)) #false)

(define (legal? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))

; List-of-posns -> List-of-posns
; from a given list create a new list which contains only posns whose x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200
(check-expect (legal '()) '())

(check-expect (legal (cons (make-posn 0 0) '())) (cons (make-posn 0 0) '()))
(check-expect (legal (cons (make-posn 1 2) '())) (cons (make-posn 1 2) '()))
(check-expect (legal (cons (make-posn 100 200) '())) (cons (make-posn 100 200) '()))
(check-expect (legal (cons (make-posn 0 0) (cons (make-posn 1 2) '())))
              (cons (make-posn 0 0) (cons (make-posn 1 2) '())))

(check-expect (legal (cons (make-posn -1 0) '())) '())
(check-expect (legal (cons (make-posn 101 0) '())) '())
(check-expect (legal (cons (make-posn 0 -1) '())) '())
(check-expect (legal (cons (make-posn 0 201) '())) '())
(check-expect (legal (cons (make-posn -1 0) (cons (make-posn 5 300) '()))) '())

(check-expect (legal (cons (make-posn -1 0) (cons (make-posn 1 2) '())))
              (cons (make-posn 1 2) '()))
(check-expect (legal (cons (make-posn 0 0) (cons (make-posn 1 202) '())))
              (cons (make-posn 0 0) '()))

(define (legal l)
  (cond
    [(empty? l) '()]
    [else (if (legal? (first l))
              (cons (first l) (legal (rest l)))
              (legal (rest l)))]))
