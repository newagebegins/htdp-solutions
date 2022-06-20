;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |195|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; On OS X:
(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; count how many words in the given dictionary start with the given letter
(check-expect (starts-with# "a" '()) 0)
(check-expect (starts-with# "a" (list "abu")) 1)
(check-expect (starts-with# "a" (list "bob")) 0)
(check-expect (starts-with# "a" (list "fun" "ace")) 1)
(check-expect (starts-with# "a" (list "ace" "fun")) 1)
(check-expect (starts-with# "a" (list "ace" "bob" "abu")) 2)
(check-expect (starts-with# "d" (list "fun" "day" "dog" "doom")) 3)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (+ (if (string=? (string-ith (first d) 0) l)
                 1
                 0)
             (starts-with# l (rest d)))]))

; > (starts-with# "e" AS-LIST)
; 7818

; > (starts-with# "z" AS-LIST)
; 719
