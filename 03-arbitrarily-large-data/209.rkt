;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |209|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Word is one of:
; - '() or
; - (cons 1String Word)
; interp. a Word is a list of 1Strings (letters)

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "") '())
(check-expect (string->word "a") (list "a"))
(check-expect (string->word "ab") (list "a" "b"))
(check-expect (string->word "abc") (list "a" "b" "c"))

(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a")) "a")
(check-expect (word->string (list "a" "b")) "ab")
(check-expect (word->string (list "a" "b" "c")) "abc")

(define (word->string w)
  (implode w))
