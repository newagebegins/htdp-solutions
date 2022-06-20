;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |210|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define S1 "hello")
(define S2 "racket")
(define S3 "rat")
(define W1 (string->word S1))
(define W2 (string->word S2))
(define W3 (string->word S3))

; List-of-words -> List-of-strings
; turn all Words in low into Strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list W1)) (list S1))
(check-expect (words->strings (list W1 W2)) (list S1 S2))
(check-expect (words->strings (list W3 W1 W2)) (list S3 S1 S2))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))
