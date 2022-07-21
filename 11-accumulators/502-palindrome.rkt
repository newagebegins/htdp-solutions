;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 502-palindrome) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect (palindrome (explode "c")) (explode "c"))
(check-expect (palindrome (explode "abc")) (explode "abcba"))

(define (palindrome s0)
  (local (; [NEList-of 1String] [List-of 1String] -> [NEList-of 1String]
          ; accumulator: a is a reversed list of letters from s0 that s lacks
          (define (palindrome/a s a)
            (cond
              [(empty? (rest s)) (append s0 a)]
              [else (palindrome/a (rest s) (cons (first s) a))])))
    (palindrome/a s0 '())))
