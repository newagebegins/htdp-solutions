;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |212|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define W1 (list "c" "a" "t"))
(define W2 (list "h" "e" "l" "l" "o"))

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)
(define LW1 (list W1 W2))
