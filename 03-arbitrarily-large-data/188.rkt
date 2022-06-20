;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |188|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; An Email is a structure:
;   (make-email String Number String)
; interp. (make-email f d m) represents text m sent by f, d seconds after the beginning of time

; A List-of-emails is one of:
; - '()
; - (cons Email List-of-emails)

; Email Email -> Boolean
; return #true when the date of e1 is strictly less than the date of e2
(check-expect (email-date<? (make-email "Alice" 5 "Hello") (make-email "Bob" 6 "World")) #true)
(check-expect (email-date<? (make-email "Alice" 6 "Hello") (make-email "Bob" 5 "World")) #false)
(check-expect (email-date<? (make-email "Alice" 5 "Hello") (make-email "Bob" 5 "World")) #false)

(define (email-date<? e1 e2)
  (< (email-date e1) (email-date e2)))

; Email List-of-emails -> List-of-emails
; inserts e into the sorted by date list of emails l
(check-expect (insert-by-date (make-email "a" 5 "m") (list (make-email "b" 10 "m") (make-email "c" 4 "m")))
              (list (make-email "b" 10 "m") (make-email "a" 5 "m") (make-email "c" 4 "m")))

(define (insert-by-date e l)
  (cond
    [(empty? l) (list e)]
    [else (if (email-date<? e (first l))
              (cons (first l) (insert-by-date e (rest l)))
              (cons e l))]))

; List-of-emails -> List-of-emails
; rearranges l in descending order by date

(check-expect (sort-by-date> '()) '())
(check-expect (sort-by-date> (list (make-email "p1" 5 "m1") (make-email "p2" 10 "m2") (make-email "p3" -2 "m3")))
              (list (make-email "p2" 10 "m2") (make-email "p1" 5 "m1") (make-email "p3" -2 "m3")))

(define (sort-by-date> l)
  (cond
    [(empty? l) '()]
    [else (insert-by-date (first l) (sort-by-date> (rest l)))]))

; Email Email -> Boolean
; return #true when the `from` of e1 is strictly less than the `from` of e2
(check-expect (email-from<? (make-email "a" 5 "m") (make-email "b" 5 "m")) #true)
(check-expect (email-from<? (make-email "b" 5 "m") (make-email "a" 5 "m")) #false)
(check-expect (email-from<? (make-email "a" 5 "m") (make-email "a" 5 "m")) #false)

(define (email-from<? e1 e2)
  (string<? (email-from e1) (email-from e2)))

; Email List-of-emails -> List-of-emails
; inserts e into the sorted by `from` list of emails l
(check-expect (insert-by-from (make-email "b" 5 "m") (list (make-email "z" 5 "m") (make-email "a" 5 "m")))
              (list (make-email "z" 5 "m") (make-email "b" 5 "m") (make-email "a" 5 "m")))

(define (insert-by-from e l)
  (cond
    [(empty? l) (list e)]
    [else (if (email-from<? e (first l))
              (cons (first l) (insert-by-from e (rest l)))
              (cons e l))]))

; List-of-emails -> List-of-emails
; rearranges l in descending order by `from`

(check-expect (sort-by-from> '()) '())
(check-expect (sort-by-from> (list (make-email "a" 5 "m") (make-email "c" 5 "m") (make-email "b" 5 "m")))
              (list (make-email "c" 5 "m") (make-email "b" 5 "m") (make-email "a" 5 "m")))

(define (sort-by-from> l)
  (cond
    [(empty? l) '()]
    [else (insert-by-from (first l) (sort-by-from> (rest l)))]))
