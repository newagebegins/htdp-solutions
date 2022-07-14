;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 453-tokenize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Line is a [List-of 1String]

; A Token is one of:
; - 1String (excluding white-space)
; - Word

; A Word is a String that consists of lower-case letters and nothing else

; Line -> Word
; extract the first word from a line
(check-expect (first-word '()) "")
(check-expect (first-word '("1")) "")
(check-expect (first-word '("\n")) "")
(check-expect (first-word '(" ")) "")
(check-expect (first-word '("a" "b" "c")) "abc")
(check-expect (first-word '("d" "e")) "de")
(check-expect (first-word '("a" "b" "c" "1")) "abc")
(check-expect (first-word '("a" "b" "c" "\n")) "abc")
(check-expect (first-word '("a" "b" "c" " ")) "abc")

(define (first-word l)
  (cond
    [(empty? l) ""]
    [(string-alphabetic? (first l)) (string-append (first l) (first-word (rest l)))]
    [else ""]))

; Line -> Line
; remove the first word from a line
(check-expect (remove-first-word '()) '())
(check-expect (remove-first-word '("a" "b" "c")) '())
(check-expect (remove-first-word '("a" "b" "c" "\n" "d" "e")) '("\n" "d" "e"))
(check-expect (remove-first-word '("a" "b" "c" " " "d" "e")) '(" " "d" "e"))
(check-expect (remove-first-word '("a" "b" "c" "1" "d" "e")) '("1" "d" "e"))
(check-expect (remove-first-word '("\n")) '("\n"))
(check-expect (remove-first-word '(" ")) '(" "))
(check-expect (remove-first-word '("1")) '("1"))

(define (remove-first-word l)
  (cond
    [(empty? l) '()]
    [(string-alphabetic? (first l)) (remove-first-word (rest l))]
    [else l]))

; Line -> [List-of Token]
; tokenize a line
(check-expect (tokenize '()) '())
(check-expect (tokenize '("a" "b" "c")) '("abc"))
(check-expect (tokenize '("\n" " " "\n")) '())
(check-expect (tokenize '("1" "2" "#" "@")) '("1" "2" "#" "@"))
(check-expect (tokenize '("a" "b" "c" "\n" "x" " " "y" "z" "\n" "\n" "1" "2" "a" "b" "#"))
              '("abc" "x" "yz" "1" "2" "ab" "#"))

(define (tokenize l)
  (cond
    [(empty? l) '()]
    [(string-whitespace? (first l)) (tokenize (rest l))]
    [(string-alphabetic? (first l)) (cons (first-word l) (tokenize (remove-first-word l)))]
    [else (cons (first l) (tokenize (rest l)))]))
