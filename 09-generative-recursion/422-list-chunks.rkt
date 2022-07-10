;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 422-list-chunks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List N -> List
; returns the first n items of l
(check-expect (take '() 1) '())
(check-expect (take '(a b c) 0) '())
(check-expect (take '(a b c) 1) '(a))
(check-expect (take '(a b c) 2) '(a b))
(check-expect (take '(a b c) 3) '(a b c))
(check-expect (take '(a b c) 4) '(a b c))

(define (take l n)
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; List N -> List
; removes the first n items of l
(check-expect (drop '() 2) '())
(check-expect (drop '(a b c) 0) '(a b c))
(check-expect (drop '(a b c) 1) '(b c))
(check-expect (drop '(a b c) 2) '(c))
(check-expect (drop '(a b c) 3) '())
(check-expect (drop '(a b c) 4) '())

(define (drop l n)
  (cond
    [(empty? l) '()]
    [(zero? n) l]
    [else (drop (rest l) (sub1 n))]))

; List N -> [List-of List]
(check-expect (list->chunks '() 1) '())
(check-expect (list->chunks '(a b c d) 1) '((a) (b) (c) (d)))
(check-expect (list->chunks '(a b c d) 2) '((a b) (c d)))
(check-expect (list->chunks '(a b c d) 3) '((a b c) (d)))
(check-expect (list->chunks '(a b c d) 4) '((a b c d)))
(check-expect (list->chunks '(a b c d) 5) '((a b c d)))

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else (cons (take l n) (list->chunks (drop l n) n))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle (explode "abcdefg") 3) (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

(define (bundle s n)
  (map implode (list->chunks s n)))
