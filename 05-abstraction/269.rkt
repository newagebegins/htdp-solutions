;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |269|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
; interp. an inventory record

; Number [List-of IR] -> [List-of IR]
; produce a list of records whose price is below ua
(check-expect (eliminate-expensive 10 '()) '())
(check-expect (eliminate-expensive 10 (list (make-IR "a" 9)))
              (list (make-IR "a" 9)))
(check-expect (eliminate-expensive 10 (list (make-IR "a" 11)))
              '())
(check-expect (eliminate-expensive 10 (list (make-IR "a" 9) (make-IR "b" 10)
                                            (make-IR "c" 5) (make-IR "c" 10.5)))
              (list (make-IR "a" 9) (make-IR "c" 5)))

(define (eliminate-expensive ua l)
  (local (; IR -> Boolean
          (define (keep? r)
            (< (IR-price r) ua)))
    (filter keep? l)))

; String [List-of IR] -> [List-of IR]
; remove records with the name ty from the list l
(check-expect (recall "foo" (list (make-IR "abc" 1) (make-IR "foo" 2)
                                  (make-IR "def" 3) (make-IR "foo" 4)))
              (list (make-IR "abc" 1) (make-IR "def" 3)))

(define (recall ty l)
  (local (; IR -> Boolean
          (define (keep? r)
            (not (string=? (IR-name r) ty))))
    (filter keep? l)))

; [List-of String] [List-of String] -> [List-of String]
; consumes two lists of names and selects all those from the second one that are also on the first
(check-expect (selection '("a" "b") '("a" "c" "b" "a" "d" "b" "e"))
              '("a" "b" "a" "b"))
(check-expect (selection '() '("a" "c" "b" "a" "d" "b" "e")) '())
(check-expect (selection '("foo") '("a" "c" "foo" "a" "d" "b" "e"))
              '("foo"))
(check-expect (selection '("foo" "bar") '("a" "c" "foo" "a" "d" "b" "e"))
              '("foo"))
(check-expect (selection '("foo" "bar") '("a" "bar" "foo" "e"))
              '("bar" "foo"))

(define (selection l1 l2)
  (local (; String -> Boolean
          (define (keep? s)
            (member? s l1)))
    (filter keep? l2)))
