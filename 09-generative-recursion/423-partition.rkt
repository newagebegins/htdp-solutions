;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 423-partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String N -> [List-of String]
(check-expect (partition "" 3) '())
(check-expect (partition "abcd" 1) '("a" "b" "c" "d"))
(check-expect (partition "abcd" 2) '("ab" "cd"))
(check-expect (partition "abcd" 3) '("abc" "d"))
(check-expect (partition "abcd" 4) '("abcd"))
(check-expect (partition "abcd" 5) '("abcd"))

(define (partition s n)
  (cond
    [(string=? s "") '()]
    [(< (string-length s) n) (list s)]
    [else (cons (substring s 0 n) (partition (substring s n) n))]))
